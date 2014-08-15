open Lwt

open Ypotryll.Methods
open Ngamia_message


let printlf_pink format =
  Lwt_io.printlf ("\x1b[35m" ^^ format ^^ "\x1b[0m")


let basic_publish channel exchange routing_key content =
  Basic.publish channel ~exchange ~routing_key ~mandatory:true
    ~immediate:false (Ypotryll_contents.Basic.make_t ()) content


let publish_msg channel msg =
  basic_publish channel "vumi" "telnet.outbound"
    (Transport_user_message.to_json_string msg)


let reverse_string text =
  let len = (String.length text) in
  let txet = String.create len in
  String.iteri (fun i c -> txet.[len - i - 1] <- c) text;
  txet


let process_message channel msg =
  match msg.Transport_user_message.content with
  | None -> return_unit
  | Some content ->
    let content = reverse_string content in
    let reply = Transport_user_message.reply msg (Some content) in
    publish_msg channel reply


let handle_deliver channel record (properties, body) =
  let delivery_tag = record.Ypotryll_methods.Basic_deliver.delivery_tag in
  printlf_pink "dtag: %Lu" delivery_tag >>
  let msg = Transport_user_message.from_json_string body in
  process_message channel msg
  >> Basic.ack channel ~delivery_tag ~multiple:false ()


let rec catch_frames channel =
  Ypotryll.get_method_with_content channel
  >>= function
  | None -> return_unit
  | Some (`Basic_deliver record as payload, Some content) ->
    handle_deliver channel record content >>
    catch_frames channel
  | Some (payload, content) -> catch_frames channel


let exchange_declare channel exchange type_ =
  Exchange.declare channel
    ~exchange ~type_ ~passive:false ~durable:true ~no_wait:false ~arguments:[]
    ()
  >>= fun _ ->
  printlf_pink "Exchange created: %s" exchange


let queue_declare channel queue =
  Queue.declare channel
    ~queue ~passive:false ~durable:true ~exclusive:false ~auto_delete:false
    ~no_wait:false ~arguments:[] ()
  >>= fun { Ypotryll_methods.Queue_declare_ok.queue } ->
  printlf_pink "queue created: %s" queue >>
  return queue


let queue_bind channel queue exchange routing_key =
  Queue.bind channel
    ~queue ~exchange ~routing_key ~no_wait:false ~arguments:[] ()
  >>= fun _ ->
  printlf_pink "queue bound to exchange: %s <- %s : %s"
    queue exchange routing_key


let basic_consume channel queue consumer_tag =
  Basic.consume channel
    ~queue ~consumer_tag ~no_local:false ~no_ack:false ~exclusive:false
    ~no_wait:false ~arguments:[] ()
  >>= fun { Ypotryll_methods.Basic_consume_ok.consumer_tag } ->
  printlf_pink "consumer created: %s" consumer_tag >>
  return consumer_tag


let do_stuff client =
  try_lwt
    printlf_pink "hello" >>
    lwt channel = Ypotryll.open_channel client in
    ignore_result (catch_frames channel);
    exchange_declare channel "vumi" "direct" >>
    queue_declare channel "telnet.inbound"
    >>= fun queue_name ->
    queue_bind channel queue_name "vumi" "telnet.inbound" >>
    basic_consume channel queue_name ""
    >>= fun consumer_tag ->
    Lwt_unix.sleep 60. >>
    Ypotryll.close_channel channel
  finally Ypotryll.close_connection client


let lwt_main =
  let params =
    Ypotryll.make_params ~username:"vumi" ~password:"vumi"
      ~virtual_host:"/develop" ()
  in
  lwt client = Ypotryll.connect ~server:"localhost" ~params () in
  try_lwt
    do_stuff client <&> Ypotryll.wait_for_shutdown client
  with Failure text -> Lwt_io.printlf "exception: %S" text >> return_unit


let () = Lwt_main.run lwt_main
