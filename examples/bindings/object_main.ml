open Object_bindings

class d = object
  inherit c as mom
  method f s = mom#f ("SUPER " ^ s)
end

let _= 
  let obj = new d in
    obj#g "test"
