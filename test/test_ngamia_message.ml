open OUnit2

open Ngamia_message


let assert_strings_equal = assert_equal ~printer:(Printf.sprintf "%S")
let assert_jsons_equal = assert_equal ~printer:Yojson.Basic.to_string


let transport_message_tests =
  "test_transport_message" >::: [

    "test_from_json_small_valid" >:: (fun ctx ->
        let tm = Transport_message.from_json (`Assoc [
            "message_version", `String "20110921";
            "message_type", `String "toy";
            "timestamp", `String "2014-08-15 18:09:00";
            "routing_metadata", `Assoc [];
            "helper_metadata", `Assoc [];
          ])
        in
        assert_equal `V20110921 tm.Transport_message.message_version;
        assert_equal (`Other "toy") tm.Transport_message.message_type;
        assert_strings_equal
          "2014-08-15 18:09:00" tm.Transport_message.timestamp;
        assert_jsons_equal (`Assoc []) tm.Transport_message.routing_metadata;
        assert_jsons_equal (`Assoc []) tm.Transport_message.helper_metadata
      );

    "test_to_json_small_valid" >:: (fun ctx ->
        let json = Transport_message.to_json {
            Transport_message.message_version = `V20110921;
            Transport_message.message_type = `Other "toy";
            Transport_message.timestamp = "2014-08-15 18:09:00";
            Transport_message.routing_metadata = `Assoc [];
            Transport_message.helper_metadata = `Assoc [];
          }
        in
        assert_jsons_equal (`Assoc [
            "message_version", `String "20110921";
            "message_type", `String "toy";
            "timestamp", `String "2014-08-15 18:09:00";
            "routing_metadata", `Assoc [];
            "helper_metadata", `Assoc [];
          ]) json
      );

    (* TODO: More tests. *)
  ]


let transport_user_message_tests =
  "test_transport_user_message" >::: [

    "test_from_json_small_valid" >:: (fun ctx ->
        let tm = Transport_user_message.from_json (`Assoc [
            "message_version", `String "20110921";
            "message_type", `String "user_message";
            "timestamp", `String "2014-08-15 18:09:00";
            "routing_metadata", `Assoc [];
            "helper_metadata", `Assoc [];

            "message_id", `String "abc123";
            "to_addr", `Null;
            "from_addr", `Null;
            "in_reply_to", `Null;
            "session_event", `Null;
            "content", `Null;
            "transport_name", `Null;
            "transport_type", `Null;
            "transport_metadata", `Assoc [];
            "group", `Null;
          ])
        in
        assert_equal `V20110921 tm.Transport_user_message.message_version;
        assert_equal `User_message tm.Transport_user_message.message_type;
        assert_strings_equal
          "2014-08-15 18:09:00" tm.Transport_user_message.timestamp;
        assert_jsons_equal
          (`Assoc []) tm.Transport_user_message.routing_metadata;
        assert_jsons_equal
          (`Assoc []) tm.Transport_user_message.helper_metadata;

        assert_strings_equal "abc123" tm.Transport_user_message.message_id;
        assert_equal None tm.Transport_user_message.to_addr;
        assert_equal None tm.Transport_user_message.from_addr;
        assert_equal None tm.Transport_user_message.in_reply_to;
        assert_equal None tm.Transport_user_message.session_event;
        assert_equal None tm.Transport_user_message.content;
        assert_equal None tm.Transport_user_message.transport_name;
        assert_equal None tm.Transport_user_message.transport_type;
        assert_jsons_equal
          (`Assoc []) tm.Transport_user_message.transport_metadata;
        assert_equal None tm.Transport_user_message.group;
      );

    "test_to_json_small_valid" >:: (fun ctx ->
        let json = Transport_user_message.to_json {
            Transport_user_message.message_version = `V20110921;
            Transport_user_message.message_type = `User_message;
            Transport_user_message.timestamp = "2014-08-15 18:09:00";
            Transport_user_message.routing_metadata = `Assoc [];
            Transport_user_message.helper_metadata = `Assoc [];

            Transport_user_message.message_id = "abc123";
            Transport_user_message.to_addr = None;
            Transport_user_message.from_addr = None;
            Transport_user_message.in_reply_to = None;
            Transport_user_message.session_event = None;
            Transport_user_message.content = None;
            Transport_user_message.transport_name = None;
            Transport_user_message.transport_type = None;
            Transport_user_message.transport_metadata = `Assoc [];
            Transport_user_message.group = None;
          }
        in
        assert_jsons_equal (`Assoc [
            "message_version", `String "20110921";
            "message_type", `String "user_message";
            "timestamp", `String "2014-08-15 18:09:00";
            "routing_metadata", `Assoc [];
            "helper_metadata", `Assoc [];

            "message_id", `String "abc123";
            "to_addr", `Null;
            "from_addr", `Null;
            "in_reply_to", `Null;
            "session_event", `Null;
            "content", `Null;
            "transport_name", `Null;
            "transport_type", `Null;
            "transport_metadata", `Assoc [];
            "group", `Null;
          ]) json
      );

    (* TODO: More tests. *)
  ]


let tests =
  "test_ngamia_message" >::: [
    transport_message_tests;
    transport_user_message_tests;
    (* TODO: More tests. *)
  ]
