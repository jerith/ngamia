(* Message format stuff. *)


exception Message_parse_error of string


type message_version = [
  | `V20110921
]


type message_type = [
  | `User_message
  | `Event
  | `Other of string
]


let mpe msg =
  raise (Message_parse_error msg)


let _assoc_opt k list =
  try Some (List.assoc k list) with Not_found -> None


let get_json_field_opt name = function
  | `Assoc list -> _assoc_opt name list
  | json ->
    mpe ("Can't get field from non-object: " ^ Yojson.Basic.to_string json)


let get_json_field name json =
  match get_json_field_opt name json with
  | Some value -> value
  | None ->
    mpe ("Field \"" ^ name ^ "\" missing: " ^ Yojson.Basic.to_string json)


let get_field_string name json =
  match get_json_field name json with
  | `String value -> value
  | field ->
    mpe ("Field \"" ^ name ^ "\" is not a string: "
         ^ Yojson.Basic.to_string field)


let get_field_string_opt name json =
  match get_json_field name json with
  | `String value -> Some value
  | `Null -> None
  | field ->
    mpe ("Field \"" ^ name ^ "\" is not a string or null: "
         ^ Yojson.Basic.to_string field)


let get_message_version json =
  match get_field_string "message_version" json with
  | "20110921" -> `V20110921
  | version -> mpe ("Unknown message version: " ^ version)


let get_message_type json =
  match get_field_string "message_type" json with
  | "user_message" -> `User_message
  | "event" -> `Event
  | message_type -> `Other message_type


let string_of_message_version = function
  | `V20110921 -> "20110921"


let string_of_message_type = function
  | `User_message -> "user_message"
  | `Event -> "event"
  | `Other value -> value


let string_or_null = function
  | None -> `Null
  | Some value -> `String value


module Transport_message = struct
  type t = {
    message_version: [`V20110921];
    message_type: message_type;
    timestamp: string;
    routing_metadata: Yojson.Basic.json;
    helper_metadata: Yojson.Basic.json;
  }

  let from_json json =
    {
      message_version = get_message_version json;
      message_type = get_message_type json;
      timestamp = get_field_string "timestamp" json;
      routing_metadata = get_json_field "routing_metadata" json;
      helper_metadata = get_json_field "helper_metadata" json;
    }

  let to_json t =
    `Assoc [
      "message_version", `String (string_of_message_version t.message_version);
      "message_type", `String (string_of_message_type t.message_type);
      "timestamp", `String t.timestamp;
      "routing_metadata", t.routing_metadata;
      "helper_metadata", t.helper_metadata;
    ]

  let from_json_string str =
    from_json (Yojson.Basic.from_string str)

  let to_json_string t =
    Yojson.Basic.to_string (to_json t)
end


module Transport_user_message = struct
  type t = {
    message_version: [`V20110921];
    message_type: [`User_message];
    timestamp: string;
    routing_metadata: Yojson.Basic.json;
    helper_metadata: Yojson.Basic.json;

    message_id: string;
    to_addr: string option;
    from_addr: string option;
    in_reply_to: string option;
    session_event: string option;
    content: string option;
    transport_name: string option;
    transport_type: string option;
    transport_metadata: Yojson.Basic.json;
    group: string option;
  }

  let from_json json =
    let message_type =
      match get_message_type json with
      | `User_message -> `User_message
      | `Event -> mpe "Expected user_message, got: event"
      | `Other msg_type -> mpe ("Expected user_message, got: " ^ msg_type)
    in
    {
      message_version = get_message_version json;
      message_type;
      timestamp = get_field_string "timestamp" json;
      routing_metadata = get_json_field "routing_metadata" json;
      helper_metadata = get_json_field "helper_metadata" json;

      message_id = get_field_string "message_id" json;
      to_addr = get_field_string_opt "to_addr" json;
      from_addr = get_field_string_opt "from_addr" json;
      in_reply_to = get_field_string_opt "in_reply_to" json;
      session_event = get_field_string_opt "session_event" json;
      content = get_field_string_opt "content" json;
      transport_name = get_field_string_opt "transport_name" json;
      transport_type = get_field_string_opt "transport_type" json;
      transport_metadata = get_json_field "transport_metadata" json;
      group = get_field_string_opt "group" json;
    }

  let to_json t =
    `Assoc [
      "message_version", `String (string_of_message_version t.message_version);
      "message_type", `String (string_of_message_type t.message_type);
      "timestamp", `String t.timestamp;
      "routing_metadata", t.routing_metadata;
      "helper_metadata", t.helper_metadata;

      "message_id", `String t.message_id;
      "to_addr", string_or_null t.to_addr;
      "from_addr", string_or_null t.from_addr;
      "in_reply_to", string_or_null t.in_reply_to;
      "session_event", string_or_null t.session_event;
      "content", string_or_null t.content;
      "transport_name", string_or_null t.transport_name;
      "transport_type", string_or_null t.transport_type;
      "transport_metadata", t.transport_metadata;
      "group", string_or_null t.group;
    ]

  let from_json_string str =
    from_json (Yojson.Basic.from_string str)

  let to_json_string t =
    Yojson.Basic.to_string (to_json t)

  let reply t content =
    { t with to_addr = t.from_addr; from_addr = t.to_addr; content }
end
