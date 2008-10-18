(* Js is the module containing the JavaScript/DOM interface
 * It is included in the modified standard library provided in the package *)
open Js ;;

(* alert simply binds the JavaScript function window.alert (); *)
alert "Hello World" ;;

(* At this point, the VM is stopped.
 * However, it can be resumed by events as we'll show later *)
