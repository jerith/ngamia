(* Message format stuff. *)


type message_version = [
  | `V20110921
]


type message_type = [
  | `User_message
  | `Event
  | `Other of string
]


module Transport_message : sig
  type t = {
    message_version: [`V20110921];
    message_type: message_type;
    timestamp: string;
    routing_metadata: Yojson.Basic.json;
    helper_metadata: Yojson.Basic.json;
  }

  val from_json : Yojson.Basic.json -> t
  val to_json : t -> Yojson.Basic.json
end


module Transport_user_message : sig
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

  val from_json : Yojson.Basic.json -> t
  val to_json : t -> Yojson.Basic.json
end
