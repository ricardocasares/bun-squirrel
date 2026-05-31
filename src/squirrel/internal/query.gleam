import filepath
import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import non_empty_list.{type NonEmptyList}
import simplifile
import squirrel/internal/error.{
  type Error, CannotReadFile, QueryFileHasInvalidName,
  QueryReturnsMultipleValuesWithTheSameName,
}
import squirrel/internal/gleam.{type EnumVariant, type TypeIdentifier}
import tote/bag

/// A query that still needs to go through the type checking process.
///
pub type UntypedQuery {
  UntypedQuery(
    /// The file the query comes from.
    ///
    file: String,
    /// The starting line in the source file where the query is defined.
    ///
    starting_line: Int,
    /// The name of the query, it must be a valid Gleam identifier.
    ///
    name: gleam.ValueIdentifier,
    /// Any comment lines that were preceding the query in the file.
    ///
    comment: List(String),
    /// The text of the query itself.
    ///
    content: String,
  )
}

/// This is exactly the same as an untyped query with the difference that it
/// has also been annotated with the type of its parameters and returned values.
///
pub type TypedQuery {
  TypedQuery(
    file: String,
    starting_line: Int,
    name: gleam.ValueIdentifier,
    comment: List(String),
    content: String,
    params: List(gleam.Type),
    returns: List(gleam.Field),
  )
}

/// Turns an untyped query into a typed one.
///
pub fn add_types(
  to query: UntypedQuery,
  params params: List(gleam.Type),
  returns returns: List(gleam.Field),
) -> Result(TypedQuery, Error) {
  let UntypedQuery(file:, name:, comment:, content:, starting_line:) = query

  case duplicate_names(returns) {
    [] ->
      Ok(TypedQuery(
        file:,
        name:,
        comment:,
        content:,
        starting_line:,
        params:,
        returns:,
      ))

    names ->
      Error(QueryReturnsMultipleValuesWithTheSameName(
        file:,
        content:,
        starting_line:,
        names:,
      ))
  }
}

fn duplicate_names(fields: List(gleam.Field)) -> List(String) {
  let names = {
    use bag, gleam.Field(label:, ..) <- list.fold(fields, from: bag.new())
    bag.insert(bag, 1, of: gleam.value_identifier_to_string(label))
  }

  use duplicate_names, field, copies <- bag.fold(names, from: [])
  case copies {
    1 -> duplicate_names
    _ -> [field, ..duplicate_names]
  }
}

// --- PARSING -----------------------------------------------------------------

/// Reads a query from a file.
/// This expects the user to follow the convention of having a single query per
/// file.
///
pub fn from_file(file: String) -> Result(UntypedQuery, Error) {
  let read_file =
    simplifile.read(file)
    |> result.map_error(CannotReadFile(file, _))

  use content <- result.try(read_file)

  // A query always starts at the top of the file.
  // If in the future I want to add support for many queries per file this
  // field will be handy to properly show error messages.
  let file_name =
    filepath.base_name(file)
    |> filepath.strip_extension
  let name =
    gleam.value_identifier(file_name)
    |> result.map_error(QueryFileHasInvalidName(
      file:,
      reason: _,
      suggested_name: gleam.similar_value_identifier_string(file_name)
        |> option.from_result,
    ))

  use name <- result.try(name)
  Ok(UntypedQuery(
    file:,
    starting_line: 1,
    name:,
    content:,
    comment: take_comment(content),
  ))
}

fn take_comment(query: String) -> List(String) {
  do_take_comment(query, [])
}

fn do_take_comment(query: String, lines: List(String)) -> List(String) {
  case string.trim_start(query) {
    "--" <> rest ->
      case string.split_once(rest, on: "\n") {
        Ok(#(line, rest)) -> do_take_comment(rest, [string.trim(line), ..lines])
        _ -> do_take_comment("", [string.trim(rest), ..lines])
      }
    _ -> list.reverse(lines)
  }
}

// --- CODE GENERATION ---------------------------------------------------------

const indent = 2

type CodeGenState {
  CodeGenState(
    imports: Dict(String, Set(String)),
    // The hard coded encoding/decoding helpers the generated code needs to
    // compile. Some Postgres types do not have a ready made encoder/decoder in
    // `brioche/sql` so we generate our own and add them to a final section of
    // the file.
    helpers: Set(Helper),
    // All the enums used in the module, this maps from name of the enum to a
    // list of its variants and what kind of helpers need to be generated for
    // the enum encoding/decoding.
    enums: Dict(TypeIdentifier, EnumCodeGenData),
  )
}

/// A hard coded encoding/decoding helper that gets added to the bottom of a
/// generated file when one of its queries needs it.
///
/// `DateDecoder` and `JsonDecoder` also require a companion `sql_ffi.mjs` file
/// to be generated next to the Gleam module: Bun decodes `date`s into
/// JavaScript `Date` objects and `json`/`jsonb` into already parsed objects, so
/// we need a tiny bit of FFI to turn those back into the values we expect.
///
type Helper {
  IntDecoder
  UuidDecoder
  NumericDecoder
  TimeOfDayDecoder
  DateDecoder
  JsonDecoder
  JsonEncoder
  DateEncoder
  TimeOfDayEncoder
}

/// Data needed to perform codegen for an enum.
///
type EnumCodeGenData {
  EnumCodeGenData(
    /// Needed to know what kind of functions to generate for the specific case.
    ///
    required_helpers: RequiredHelpers,
    /// The original name used to define the enum in postgres to generate a
    /// useful comment.
    ///
    original_name: String,
    /// The variants of the enum.
    ///
    variants: NonEmptyList(EnumVariant),
  )
}

type RequiredHelpers {
  NeedsEncoderAndDecoder
  NeedsDecoder
  NeedsEncoder
  NoHelpers
}

fn merge_helpers(
  one: RequiredHelpers,
  other: RequiredHelpers,
) -> RequiredHelpers {
  case one, other {
    NoHelpers, other | other, NoHelpers -> other
    NeedsEncoderAndDecoder, _ | _, NeedsEncoderAndDecoder ->
      NeedsEncoderAndDecoder
    NeedsDecoder, NeedsEncoder | NeedsEncoder, NeedsDecoder ->
      NeedsEncoderAndDecoder
    NeedsEncoder, NeedsEncoder -> NeedsEncoder
    NeedsDecoder, NeedsDecoder -> NeedsDecoder
  }
}

fn default_codegen_state() {
  CodeGenState(imports: dict.new(), helpers: set.new(), enums: dict.new())
  |> import_module("gleam/dynamic/decode")
  |> import_module("gleam/javascript/promise")
  |> import_module("brioche/sql")
}

fn add_helper(state: CodeGenState, helper: Helper) -> CodeGenState {
  CodeGenState(..state, helpers: set.insert(state.helpers, helper))
}

fn gleam_type_to_decoder(
  state: CodeGenState,
  type_: gleam.Type,
) -> #(CodeGenState, Document) {
  case type_ {
    gleam.Uuid -> {
      let state = add_helper(state, UuidDecoder) |> import_module("youid/uuid")
      #(state, doc.from_string("uuid_decoder()"))
    }
    gleam.List(type_) -> {
      let #(state, inner_decoder) = gleam_type_to_decoder(state, type_)
      #(state, call_doc("decode.list", [inner_decoder]))
    }
    gleam.Option(type_) -> {
      let #(state, inner_decoder) = gleam_type_to_decoder(state, type_)
      #(state, call_doc("decode.optional", [inner_decoder]))
    }
    gleam.Date -> {
      let state =
        add_helper(state, DateDecoder)
        |> import_module("gleam/time/calendar")
      #(state, doc.from_string("date_decoder()"))
    }
    gleam.TimeOfDay -> {
      let state =
        add_helper(state, TimeOfDayDecoder)
        |> import_module("gleam/int")
        |> import_module("gleam/string")
        |> import_module("gleam/time/calendar")
      #(state, doc.from_string("time_of_day_decoder()"))
    }
    gleam.Timestamp -> #(state, doc.from_string("sql.timestamp_decoder()"))
    gleam.Int -> {
      let state = add_helper(state, IntDecoder) |> import_module("gleam/int")
      #(state, doc.from_string("int_decoder()"))
    }
    gleam.Float -> #(state, doc.from_string("decode.float"))
    gleam.Numeric -> {
      let state =
        add_helper(state, NumericDecoder)
        |> import_module("gleam/float")
        |> import_module("gleam/int")
      #(state, doc.from_string("numeric_decoder()"))
    }
    gleam.Bool -> #(state, doc.from_string("decode.bool"))
    gleam.String -> #(state, doc.from_string("decode.string"))
    gleam.BitArray -> #(state, doc.from_string("decode.bit_array"))
    gleam.Json -> {
      let state = add_helper(state, JsonDecoder)
      #(state, doc.from_string("json_decoder()"))
    }
    gleam.Enum(original_name:, name: enum_name, variants:) -> #(
      add_enum_helpers(state, original_name, enum_name, variants, NeedsDecoder),
      doc.from_string(enum_decoder_name(enum_name) <> "()"),
    )
  }
}

fn enum_decoder_name(enum_name: TypeIdentifier) -> String {
  gleam.type_identifier_to_value_identifier(enum_name)
  |> gleam.value_identifier_to_string
  |> string.append("_decoder")
}

fn gleam_type_to_encoder(
  state: CodeGenState,
  type_: gleam.Type,
  name: String,
) -> #(CodeGenState, Document) {
  let name = doc.from_string(name)
  case type_ {
    gleam.List(type_) -> {
      let #(state, inner_encoder) = gleam_type_to_encoder(state, type_, "value")
      let map_fn = fn_doc(["value"], inner_encoder)
      let doc = call_doc("sql.array", [name, map_fn])
      #(state, doc)
    }
    gleam.Option(type_) -> {
      let #(state, inner_encoder) = gleam_type_to_encoder(state, type_, "value")
      let doc =
        call_doc("sql.nullable", [name, fn_doc(["value"], inner_encoder)])
      #(state, doc)
    }
    gleam.Uuid -> {
      let state = state |> import_module("youid/uuid")
      let doc = call_doc("sql.text", [call_doc("uuid.to_string", [name])])
      #(state, doc)
    }
    gleam.Json -> {
      // We can't encode JSON as text: Bun would treat the string as a JSON
      // string value and double encode it. Instead we hand Bun an already
      // parsed value through a bit of FFI.
      let state = add_helper(state, JsonEncoder) |> import_module("gleam/json")
      let doc = call_doc("json_value", [call_doc("json.to_string", [name])])
      #(state, doc)
    }
    gleam.Date -> {
      let state =
        add_helper(state, DateEncoder)
        |> import_module("gleam/int")
        |> import_module("gleam/string")
        |> import_module("gleam/time/calendar")
      #(state, call_doc("sql.text", [call_doc("date_to_string", [name])]))
    }
    gleam.TimeOfDay -> {
      let state =
        add_helper(state, TimeOfDayEncoder)
        |> import_module("gleam/int")
        |> import_module("gleam/string")
        |> import_module("gleam/time/calendar")
      #(
        state,
        call_doc("sql.text", [call_doc("time_of_day_to_string", [name])]),
      )
    }
    gleam.Timestamp -> #(state, call_doc("sql.timestamp", [name]))
    gleam.Int -> #(state, call_doc("sql.int", [name]))
    gleam.Float | gleam.Numeric -> #(state, call_doc("sql.float", [name]))
    gleam.Bool -> #(state, call_doc("sql.bool", [name]))
    gleam.String -> #(state, call_doc("sql.text", [name]))
    gleam.BitArray -> #(state, call_doc("sql.bytea", [name]))
    gleam.Enum(original_name:, name: enum_name, variants:) -> #(
      add_enum_helpers(state, original_name, enum_name, variants, NeedsEncoder),
      call_doc(enum_encoder_name(enum_name), [name]),
    )
  }
}

fn enum_encoder_name(enum_name: TypeIdentifier) -> String {
  gleam.type_identifier_to_value_identifier(enum_name)
  |> gleam.value_identifier_to_string
  |> string.append("_encoder")
}

type TypePosition {
  EnumField
  FunctionArgument
}

fn gleam_type_to_field_type(
  state: CodeGenState,
  type_: gleam.Type,
  position: TypePosition,
) -> #(CodeGenState, Document) {
  case type_ {
    gleam.List(type_) -> {
      let #(state, inner_type) =
        gleam_type_to_field_type(state, type_, position)
      #(state, call_doc("List", [inner_type]))
    }
    gleam.Option(type_) -> {
      let state = state |> import_qualified("gleam/option", "type Option")
      let #(state, inner_type) =
        gleam_type_to_field_type(state, type_, position)
      #(state, call_doc("Option", [inner_type]))
    }
    gleam.Uuid -> #(
      state |> import_qualified("youid/uuid", "type Uuid"),
      doc.from_string("Uuid"),
    )
    gleam.Date -> {
      let state = state |> import_qualified("gleam/time/calendar", "type Date")
      #(state, doc.from_string("Date"))
    }
    gleam.TimeOfDay -> {
      let state =
        state |> import_qualified("gleam/time/calendar", "type TimeOfDay")
      #(state, doc.from_string("TimeOfDay"))
    }
    gleam.Timestamp -> {
      let state =
        state |> import_qualified("gleam/time/timestamp", "type Timestamp")
      #(state, doc.from_string("Timestamp"))
    }
    gleam.Int -> #(state, doc.from_string("Int"))
    gleam.Float | gleam.Numeric -> #(state, doc.from_string("Float"))
    gleam.Bool -> #(state, doc.from_string("Bool"))
    gleam.String -> #(state, doc.from_string("String"))
    gleam.Json ->
      case position {
        EnumField -> #(state, doc.from_string("String"))
        FunctionArgument -> {
          let state = state |> import_qualified("gleam/json", "type Json")
          #(state, doc.from_string("Json"))
        }
      }
    gleam.BitArray -> #(state, doc.from_string("BitArray"))
    gleam.Enum(original_name:, name:, variants:) -> #(
      add_enum_helpers(state, original_name, name, variants, NoHelpers),
      gleam.type_identifier_to_string(name) |> doc.from_string,
    )
  }
}

/// The result of generating code for a `sql` directory: always a Gleam module
/// and—when one of the queries uses a type that needs FFI to decode (`date`,
/// `json`/`jsonb`)—a companion `sql_ffi.mjs` file that must be written next to
/// it.
///
pub type Generated {
  Generated(code: String, ffi: option.Option(String))
}

/// Generates the code for a single file containing a bunch of typed queries.
///
pub fn generate_code(
  version version: String,
  // The directory all the queries come from.
  for queries: List(TypedQuery),
  from directory: String,
) -> Generated {
  // We need to sort the queries before generating any code, otherwise the order
  // with which they will appear in the generated file won't be reproducible!
  // That could cause CI checks like `gleam run -m squirrel check` to fail.
  //
  let queries =
    list.sort(queries, fn(one, other) { string.compare(one.file, other.file) })

  let #(state, queries_docs) = {
    let state = default_codegen_state()
    use #(state, docs), query <- list.fold(over: queries, from: #(state, []))
    let #(state, doc) = query_doc(state, version, query)
    #(state, [doc, ..docs])
  }
  let queries_docs = list.reverse(queries_docs)

  let CodeGenState(imports:, helpers:, enums:) = state

  let utils = helpers_docs(helpers)

  // We always want to output the imports and the code for the queries.
  // But in case we also need some helpers we add a final section to our file
  // with the hard coded helpers we need for the code to compile.
  let code =
    [
      imports_doc(imports),
      doc.lines(2),
      doc.join(queries_docs, with: doc.lines(2)),
    ]
    |> doc.concat

  let code = case dict.is_empty(enums) {
    True -> code
    False ->
      [
        code,
        doc.lines(2),
        separator_comment("Enums"),
        doc.lines(2),
        enums_doc(version, enums),
      ]
      |> doc.concat
  }

  let code = case utils {
    [] -> code
    [_, ..] -> {
      [
        code,
        doc.lines(2),
        separator_comment("Encoding/decoding utils"),
        doc.lines(2),
        doc.join(utils, with: doc.lines(2)),
      ]
      |> doc.concat
    }
  }

  let code =
    doc.concat([
      doc.from_string(module_doc(version, directory)),
      doc.lines(2),
      code,
      doc.line,
    ])
    |> doc.to_string(80)

  Generated(code:, ffi: ffi_module(helpers))
}

/// Builds the hard coded encoding/decoding helpers needed by a module, in a
/// stable order so the generated code stays reproducible.
///
fn helpers_docs(helpers: Set(Helper)) -> List(Document) {
  // `date_to_string` and `time_of_day_to_string` both rely on `pad_int`, so we
  // emit it whenever either of them is needed.
  let needs_pad_int =
    set.contains(helpers, DateEncoder)
    || set.contains(helpers, TimeOfDayEncoder)

  [
    #(IntDecoder, int_decoder),
    #(UuidDecoder, uuid_decoder),
    #(NumericDecoder, numeric_decoder),
    #(TimeOfDayDecoder, time_of_day_decoder),
    #(DateDecoder, date_decoder),
    #(JsonDecoder, json_decoder),
    #(JsonEncoder, json_encoder),
    #(DateEncoder, date_to_string),
    #(TimeOfDayEncoder, time_of_day_to_string),
  ]
  |> list.filter_map(fn(pair) {
    let #(helper, code) = pair
    case set.contains(helpers, helper) {
      True -> Ok(doc.from_string(code))
      False -> Error(Nil)
    }
  })
  |> prepend_if(needs_pad_int, doc.from_string(pad_int))
}

/// The content of the companion `sql_ffi.mjs` file, or `None` if none of the
/// helpers needs FFI. Only `date` and `json`/`jsonb` decoding require it.
///
fn ffi_module(helpers: Set(Helper)) -> option.Option(String) {
  let parts =
    []
    |> prepend_if(set.contains(helpers, JsonEncoder), json_value_ffi)
    |> prepend_if(set.contains(helpers, JsonDecoder), json_to_string_ffi)
    |> prepend_if(set.contains(helpers, DateDecoder), date_from_sql_ffi)

  case parts {
    [] -> option.None
    _ -> option.Some(string.join([ffi_module_header, ..parts], with: "\n\n"))
  }
}

fn separator_comment(value: String) -> Document {
  string.pad_end("// --- " <> value <> " ", to: 80, with: "-")
  |> doc.from_string
}

fn imports_doc(imports: Dict(String, Set(String))) -> Document {
  let sorted_imports =
    dict.to_list(imports)
    |> list.sort(fn(one, other) { string.compare(one.0, other.0) })

  {
    use #(module, imported_values) <- list.map(sorted_imports)
    let import_line = doc.from_string("import " <> module)
    use <- bool.guard(when: set.is_empty(imported_values), return: import_line)

    let imported_values =
      set.to_list(imported_values)
      |> list.sort(string.compare)
      |> list.map(doc.from_string)
      |> doc.join(with: doc.break(", ", ","))
      |> doc.group

    [
      import_line,
      doc.from_string(".{"),
      [doc.soft_break, imported_values]
        |> doc.concat
        |> doc.group
        |> doc.nest(by: indent),
      doc.soft_break,
      doc.from_string("}"),
    ]
    |> doc.concat
  }
  |> doc.join(with: doc.line)
}

/// Returns the generated code and a set with the needed imports to make it
/// compile.
///
fn query_doc(
  state: CodeGenState,
  version: String,
  query: TypedQuery,
) -> #(CodeGenState, Document) {
  let TypedQuery(
    file: _,
    name:,
    content:,
    comment: _,
    params:,
    returns:,
    starting_line: _,
  ) = query

  let constructor_name =
    gleam.value_identifier_to_type_identifier(name)
    |> gleam.type_identifier_to_string
    |> string.append("Row")

  let record_result = record_doc(state, version, constructor_name, query)
  let #(state, record) = case record_result {
    Ok(#(state, record)) -> #(state, doc.append(record, doc.lines(2)))
    Error(_) -> #(state, doc.empty)
  }

  let #(state, args, encoders) = {
    let acc = #(state, [], [])
    use #(state, args, encoders), param, i <- list.index_fold(params, acc)

    let arg = "arg_" <> int.to_string(i + 1)
    let #(state, arg_type) =
      gleam_type_to_field_type(state, param, FunctionArgument)
    let #(state, encoder) = gleam_type_to_encoder(state, param, arg)

    let arg = doc.concat([doc.from_string(arg <> ": "), arg_type])
    #(state, [arg, ..args], [encoder, ..encoders])
  }
  let args = list.reverse(args)
  let encoders = list.reverse(encoders)

  let #(state, decoder) = decoder_doc(state, constructor_name, returns)
  let args = [doc.from_string("db: sql.Connection"), ..args]

  let returned = case returns {
    [] -> doc.from_string("sql.Returned(Nil)")
    _ -> call_doc("sql.Returned", [doc.from_string(constructor_name)])
  }
  let result = call_doc("Result", [returned, doc.from_string("sql.SqlError")])
  let return = call_doc("promise.Promise", [result])

  let code =
    doc.concat([
      record,
      doc.from_string(function_doc(version, query)),
      doc.line,
      fun_doc(Public, gleam.value_identifier_to_string(name), args, return, [
        let_var("decoder", decoder) |> doc.append(doc.from_string("\n")),
        string_doc(content)
          |> pipe_call_doc("sql.query", _, [])
          |> pipe_call_doc("sql.format", _, [doc.from_string("sql.Tuple")])
          |> pipe_all_encoders(encoders)
          |> pipe_call_doc("sql.returning", _, [doc.from_string("decoder")])
          |> pipe_call_doc("sql.execute", _, [doc.from_string("db")]),
      ]),
    ])

  #(state, code)
}

fn pipe_all_encoders(doc: Document, decoders: List(Document)) -> Document {
  use doc, decoder <- list.fold(over: decoders, from: doc)
  doc |> pipe_call_doc("sql.parameter", _, [decoder])
}

fn module_doc(version: String, directory: String) -> String {
  "//// This module contains the code to run the sql queries defined in
//// `" <> directory <> "`.
//// > 🐿️ This module was generated automatically using " <> version <> " of
//// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
////"
}

fn function_doc(version: String, query: TypedQuery) -> String {
  let TypedQuery(comment:, name:, file:, ..) = query
  let function_name = gleam.value_identifier_to_string(name)

  let base = case comment {
    [] -> "/// Runs the `" <> function_name <> "` query
/// defined in `" <> file <> "`."
    [_, ..] ->
      list.map(comment, string.append("/// ", _))
      |> string.join(with: "\n")
  }

  base <> "
///
/// > 🐿️ This function was generated automatically using " <> version <> " of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///"
}

/// Returns the document of a record type definition if the query warrants its
/// creation: if a query doesn't return anything, then it doesn't make sense
/// to create a new record type and this function will return an `Error`.
///
/// Otherwise it returns the document defining a commented type definition with
/// the name passed in as a parameter.
///
fn record_doc(
  state: CodeGenState,
  version: String,
  type_name: String,
  query: TypedQuery,
) -> Result(#(CodeGenState, Document), Nil) {
  let TypedQuery(name:, returns:, file:, ..) = query
  use <- bool.guard(when: returns == [], return: Error(Nil))

  let function_name = gleam.value_identifier_to_string(name)
  let record_doc =
    "/// A row you get from running the `" <> function_name <> "` query
/// defined in `" <> file <> "`.
///
/// > 🐿️ This type definition was generated automatically using " <> version <> " of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///"

  let #(state, fields) = {
    use #(state, fields), field <- list.fold(returns, from: #(state, []))
    let label =
      doc.from_string(gleam.value_identifier_to_string(field.label) <> ": ")
    let #(state, field_type) =
      gleam_type_to_field_type(state, field.type_, EnumField)
    let field = [label, field_type] |> doc.concat |> doc.group

    #(state, [field, ..fields])
  }
  let fields = list.reverse(fields)

  let result =
    [
      doc.from_string(record_doc),
      doc.line,
      [
        doc.from_string("pub type " <> type_name <> " {"),
        [doc.line, call_doc(type_name, fields)]
          |> doc.concat
          |> doc.nest(by: indent),
        doc.line,
        doc.from_string("}"),
      ]
        |> doc.concat
        |> doc.group,
    ]
    |> doc.concat

  Ok(#(state, result))
}

/// Returns the document for the definition and encoding/decoding of all enums
/// in the dictionary.
///
fn enums_doc(
  version: String,
  enums: Dict(TypeIdentifier, EnumCodeGenData),
) -> Document {
  use doc, name, enum_data <- dict.fold(enums, doc.empty)
  doc.append(doc, enum_doc(version, name, enum_data))
}

/// Returns the document with the enum definition and any additional helper that
/// might be needed to encode and decode it.
///
fn enum_doc(
  version: String,
  enum_name: TypeIdentifier,
  enum_data: EnumCodeGenData,
) -> Document {
  let EnumCodeGenData(original_name:, required_helpers:, variants:) = enum_data

  case required_helpers {
    NeedsDecoder -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
      enum_decoder_doc(enum_name, variants),
    ]
    NeedsEncoder -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
      enum_encoder_doc(enum_name, variants),
    ]
    NeedsEncoderAndDecoder -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
      enum_decoder_doc(enum_name, variants),
      enum_encoder_doc(enum_name, variants),
    ]
    NoHelpers -> [
      enum_type_definition_doc(version, enum_name, original_name, variants),
    ]
  }
  |> doc.join(with: doc.lines(2))
}

fn enum_type_definition_doc(
  version: String,
  enum_name: TypeIdentifier,
  original_name: String,
  variants: NonEmptyList(EnumVariant),
) -> Document {
  let string_enum_name = gleam.type_identifier_to_string(enum_name)
  let variants =
    non_empty_list.map(variants, fn(variant) {
      gleam.type_identifier_to_string(variant.name)
      |> doc.from_string
    })

  let enum_doc =
    "/// Corresponds to the Postgres `" <> original_name <> "` enum.
///
/// > 🐿️ This type definition was generated automatically using " <> version <> " of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///"

  doc.concat([
    doc.from_string(enum_doc),
    doc.line,
    doc.from_string("pub type " <> string_enum_name <> " "),
    block(non_empty_list.to_list(variants)),
  ])
}

fn enum_encoder_doc(
  name: TypeIdentifier,
  variants: NonEmptyList(EnumVariant),
) -> Document {
  let case_lines = {
    use variant <- non_empty_list.map(variants)
    [
      doc.from_string(gleam.type_identifier_to_string(variant.name)),
      doc.from_string(" -> "),
      string_doc(variant.string_representation),
    ]
    |> doc.concat
  }

  let var_name =
    name
    |> gleam.type_identifier_to_value_identifier
    |> gleam.value_identifier_to_string

  let case_ =
    doc.concat([
      doc.from_string("case " <> var_name <> " "),
      block(non_empty_list.to_list(case_lines)),
    ])

  let case_ = pipe_call_doc("sql.text", case_, [])

  fun_doc(
    Private,
    enum_encoder_name(name),
    [doc.from_string(var_name)],
    doc.from_string("sql.Value"),
    [case_],
  )
}

fn enum_decoder_doc(
  name: TypeIdentifier,
  variants: NonEmptyList(EnumVariant),
) -> Document {
  let success_case_lines = {
    use variant <- non_empty_list.map(variants)
    doc.concat([
      string_doc(variant.string_representation),
      doc.from_string(" -> "),
      call_doc("decode.success", [
        gleam.type_identifier_to_string(variant.name)
        |> doc.from_string,
      ]),
    ])
  }

  let failure_case_line =
    [
      doc.from_string("_ -> "),
      call_doc("decode.failure", [
        doc.from_string(gleam.type_identifier_to_string(variants.first.name)),
        string_doc(gleam.type_identifier_to_string(name)),
      ]),
    ]
    |> doc.concat

  let var_name =
    name
    |> gleam.type_identifier_to_value_identifier
    |> gleam.value_identifier_to_string

  let case_ =
    doc.concat([
      doc.from_string("case " <> var_name <> " "),
      success_case_lines
        |> non_empty_list.to_list
        |> list.append([failure_case_line])
        |> block,
    ])

  let enum_decoder_type =
    doc.from_string(
      "decode.Decoder(" <> gleam.type_identifier_to_string(name) <> ")",
    )

  fun_doc(Private, enum_decoder_name(name), [], enum_decoder_type, [
    doc.from_string("use " <> var_name <> " <- decode.then(decode.string)"),
    case_,
  ])
}

const int_decoder = "/// A decoder for `Int`s coming from a Postgres query. Bun returns 64 bit
/// integers (`bigint`/`int8`) as strings to avoid losing precision, so we
/// accept both a number and a string.
///
fn int_decoder() {
  decode.one_of(decode.int, or: [
    {
      use string <- decode.then(decode.string)
      case int.parse(string) {
        Ok(int) -> decode.success(int)
        Error(_) -> decode.failure(0, \"Int\")
      }
    },
  ])
}"

const uuid_decoder = "/// A decoder to decode `Uuid`s coming from a Postgres query.
///
fn uuid_decoder() {
  use string <- decode.then(decode.string)
  case uuid.from_string(string) {
    Ok(uuid) -> decode.success(uuid)
    Error(_) -> decode.failure(uuid.v7(), \"Uuid\")
  }
}"

const numeric_decoder = "/// A decoder to decode `numeric`s coming from a Postgres query.
///
fn numeric_decoder() {
  use string <- decode.then(decode.string)
  case float.parse(string) {
    Ok(float) -> decode.success(float)
    Error(_) ->
      case int.parse(string) {
        Ok(int) -> decode.success(int.to_float(int))
        Error(_) -> decode.failure(0.0, \"Numeric\")
      }
  }
}"

const time_of_day_decoder = "/// A decoder to decode `time`s coming from a Postgres query.
///
fn time_of_day_decoder() {
  use string <- decode.then(decode.string)
  case string.split(string, on: \":\") {
    [hours, minutes, rest] -> {
      let seconds = case string.split_once(rest, on: \".\") {
        Ok(#(seconds, _)) -> seconds
        Error(_) -> rest
      }
      case int.parse(hours), int.parse(minutes), int.parse(seconds) {
        Ok(hours), Ok(minutes), Ok(seconds) ->
          decode.success(calendar.TimeOfDay(hours, minutes, seconds, 0))
        _, _, _ -> decode.failure(calendar.TimeOfDay(0, 0, 0, 0), \"TimeOfDay\")
      }
    }
    _ -> decode.failure(calendar.TimeOfDay(0, 0, 0, 0), \"TimeOfDay\")
  }
}"

const date_decoder = "/// A decoder to decode `date`s coming from a Postgres query.
///
fn date_decoder() {
  use dynamic <- decode.then(decode.dynamic)
  let #(year, month, day) = date_from_sql(dynamic)
  case calendar.month_from_int(month) {
    Ok(month) -> decode.success(calendar.Date(year, month, day))
    Error(_) -> decode.failure(calendar.Date(1970, calendar.January, 1), \"Date\")
  }
}

@external(javascript, \"./sql_ffi.mjs\", \"dateFromSql\")
fn date_from_sql(value: decode.Dynamic) -> #(Int, Int, Int)"

const json_decoder = "/// A decoder to decode `json`/`jsonb` values coming from a Postgres query.
///
fn json_decoder() {
  use dynamic <- decode.then(decode.dynamic)
  decode.success(json_to_string(dynamic))
}

@external(javascript, \"./sql_ffi.mjs\", \"jsonToString\")
fn json_to_string(value: decode.Dynamic) -> String"

const json_encoder = "@external(javascript, \"./sql_ffi.mjs\", \"jsonValue\")
fn json_value(json: String) -> sql.Value"

const date_to_string = "/// Encodes a `Date` as the `YYYY-MM-DD` string Postgres expects.
///
fn date_to_string(date: calendar.Date) -> String {
  let calendar.Date(year, month, day) = date
  pad_int(year, 4)
  <> \"-\"
  <> pad_int(calendar.month_to_int(month), 2)
  <> \"-\"
  <> pad_int(day, 2)
}"

const time_of_day_to_string = "/// Encodes a `TimeOfDay` as the `HH:MM:SS` string Postgres expects.
///
fn time_of_day_to_string(time: calendar.TimeOfDay) -> String {
  let calendar.TimeOfDay(hours, minutes, seconds, _) = time
  pad_int(hours, 2) <> \":\" <> pad_int(minutes, 2) <> \":\" <> pad_int(seconds, 2)
}"

const pad_int = "fn pad_int(value: Int, length: Int) -> String {
  string.pad_start(int.to_string(value), to: length, with: \"0\")
}"

/// A decoder that discards its value and always returns `Nil` instead.
///
const nil_decoder = "decode.map(decode.dynamic, fn(_) { Nil })"

const ffi_module_header = "// This file was generated automatically using squirrel.
// It contains the bits of FFI needed to decode the types that Bun returns as
// JavaScript values rather than strings."

const date_from_sql_ffi = "export function dateFromSql(date) {
  return [date.getUTCFullYear(), date.getUTCMonth() + 1, date.getUTCDate()]
}"

const json_to_string_ffi = "export function jsonToString(value) {
  return JSON.stringify(value)
}"

const json_value_ffi = "export function jsonValue(json) {
  return JSON.parse(json)
}"

/// A pretty printed decoder that decodes an n-item dynamic tuple with the given
/// constructor wrapping the returned rows from a query.
///
/// If the query returns no columns (that is `returns == []`), then we default
/// to building decoder that always returns `Nil`.
///
fn decoder_doc(
  state: CodeGenState,
  constructor: String,
  returns: List(gleam.Field),
) -> #(CodeGenState, Document) {
  let fallback = #(state, doc.from_string(nil_decoder))
  use <- bool.guard(when: returns == [], return: fallback)

  let #(state, parameters, labelled_names) = {
    use acc, field, i <- list.index_fold(returns, #(state, [], []))
    let #(state, parameters, labelled_names) = acc

    let label = gleam.value_identifier_to_string(field.label)
    let labelled_names = [doc.from_string(label <> ":"), ..labelled_names]

    let position = int.to_string(i) |> doc.from_string
    let #(state, decoder) = gleam_type_to_decoder(state, field.type_)
    let param =
      doc.from_string("use " <> label <> " <- ")
      |> doc.append(call_doc("decode.field", [position, decoder]))
    let parameters = [param, ..parameters]

    #(state, parameters, labelled_names)
  }
  let parameters = list.reverse(parameters)
  let labelled_names = list.reverse(labelled_names)

  let success_line =
    nested_calls_doc("decode.success", constructor, labelled_names)

  let doc = block(list.append(parameters, [success_line]))
  #(state, doc)
}

/// A pretty printed function call where the first argument is piped into
/// the function.
///
fn pipe_call_doc(
  function: String,
  first: Document,
  rest: List(Document),
) -> Document {
  let function = case rest {
    [] -> doc.from_string("|> " <> function)
    [_, ..] -> call_doc("|> " <> function, rest)
  }

  [first, doc.line, function]
  |> doc.concat
}

/// A pretty printed function call.
///
fn call_doc(function: String, args: List(Document)) -> Document {
  [doc.from_string(function), comma_list("(", args, ")") |> doc.group]
  |> doc.concat
  |> doc.group
}

/// This is a special case of a call document. To accomodate for a special rule
/// of the Gleam formatter: when we have a function call that has a single other
/// function as its one and only argument.
///
/// ```gleam
/// first(second(arg_1, arg_2, arg_3, ..., arg_n))
/// ```
///
/// When this needs to be broken, the formatter will only split the arguments of
/// the second call like this:
///
/// ```gleam
/// first(second(
///   arg_1,
///   ...,
///   arg_n
/// ))
/// ```
///
/// Given the first and second function, and the arguments of the second
/// function, this function builds a document that behaves like that.
///
fn nested_calls_doc(
  first: String,
  second: String,
  arguments: List(Document),
) -> Document {
  [
    doc.from_string(first),
    doc.from_string("("),
    // ^^ For the first call we don't add any breakable space after the `(`, so
    //    that the only thing that can get broken on multiple lines are the
    //    arguments of the second function
    call_doc(second, arguments),
    // ^^ And the second call is broken and behaves as usual, with its arguments
    //    being nested
    doc.from_string(")"),
  ]
  |> doc.concat
}

/// A pretty printed Gleam block.
///
fn block(body: List(Document)) -> Document {
  [
    doc.from_string("{"),
    doc.line |> doc.nest(by: indent),
    body
      |> doc.join(with: doc.line)
      |> doc.nest(by: indent),
    doc.line,
    doc.from_string("}"),
  ]
  |> doc.concat
}

type Publicity {
  Public
  Private
}

/// A pretty printed public function definition.
///
fn fun_doc(
  publicity: Publicity,
  name: String,
  args: List(Document),
  return_type: Document,
  body: List(Document),
) -> Document {
  let publicity = case publicity {
    Private -> ""
    Public -> "pub "
  }

  [
    [
      doc.from_string(publicity <> "fn " <> name),
      comma_list("(", args, ")"),
      doc.from_string(" -> "),
      return_type,
      doc.from_string(" "),
    ]
      |> doc.concat
      |> doc.group,
    block(body),
  ]
  |> doc.concat
  |> doc.group
}

fn fn_doc(args: List(String), body: Document) -> Document {
  [
    doc.from_string("fn"),
    comma_list("(", list.map(args, doc.from_string), ") {")
      |> doc.group,
    [doc.space, body]
      |> doc.concat
      |> doc.nest(by: indent),
    doc.space,
    doc.from_string("}"),
  ]
  |> doc.concat
  |> doc.group
}

/// A pretty printed let assignment.
///
fn let_var(name: String, body: Document) -> Document {
  [doc.from_string("let " <> name <> " ="), doc.space, body]
  |> doc.concat
  |> doc.group
}

/// A pretty printed Gleam string.
///
/// > ⚠️ This function escapes all `\` and `"` inside the original string to
/// > avoid generating invalid Gleam code.
///
fn string_doc(content: String) -> Document {
  let escaped_string =
    content
    |> string.replace(each: "\\", with: "\\\\")
    |> string.replace(each: "\"", with: "\\\"")
    |> doc.from_string

  [doc.from_string("\""), escaped_string, doc.from_string("\"")]
  |> doc.concat
}

/// A comma separated list of items with some given open and closed delimiters.
///
fn comma_list(
  open: String,
  content: List(Document),
  close: String,
) -> Document {
  case content {
    [] -> doc.from_string(open <> close)
    _ ->
      [
        doc.from_string(open),
        [
          // We want the first break to be nested
          // in case the group is broken.
          doc.soft_break,
          doc.join(content, doc.break(", ", ",")),
        ]
          |> doc.concat
          |> doc.nest(by: indent),
        doc.break("", ","),
        doc.from_string(close),
      ]
      |> doc.concat
  }
}

// --- UTILS TO WORK WITH STATE ------------------------------------------------

fn import_module(state: CodeGenState, name: String) -> CodeGenState {
  let imports = case dict.has_key(state.imports, name) {
    False -> dict.insert(state.imports, name, set.new())
    True -> state.imports
  }
  CodeGenState(..state, imports:)
}

fn import_qualified(
  state: CodeGenState,
  module: String,
  imported: String,
) -> CodeGenState {
  let imports =
    dict.upsert(state.imports, module, fn(imported_values) {
      case imported_values {
        Some(imported_values) -> set.insert(imported_values, imported)
        None -> set.from_list([imported])
      }
    })

  CodeGenState(..state, imports:)
}

fn add_enum_helpers(
  state: CodeGenState,
  original_name: String,
  name: TypeIdentifier,
  variants: NonEmptyList(EnumVariant),
  required_helpers: RequiredHelpers,
) -> CodeGenState {
  CodeGenState(..state, enums: {
    use value <- dict.upsert(state.enums, name)
    case value {
      None -> EnumCodeGenData(required_helpers:, variants:, original_name:)
      Some(EnumCodeGenData(required_helpers: helpers, ..) as data) -> {
        let required_helpers = merge_helpers(required_helpers, helpers)
        EnumCodeGenData(..data, required_helpers:)
      }
    }
  })
}

// --- MISC UTILS --------------------------------------------------------------

fn prepend_if(list: List(a), condition: Bool, item: a) -> List(a) {
  case condition {
    True -> [item, ..list]
    False -> list
  }
}
