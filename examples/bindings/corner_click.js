function corner_click (unit) {
  // 'this' is not a variable but a keyword, so we need to bind it to a variable
  // for it to be included in closures
  var vm = this;

  // we need a resource for wait and notify
  // we will also use it to pass values from the event to the continuation
  var res = [];

  try {
    document.getElementById("corner").onclick = function (evt) {
      // resume the thread on click
      res.x = evt.clientX;
      res.y = evt.clientY;
      vm.thread_notify_all (res);
    }

    // this is the function to be called when the thread resumes
    var cont = function  () {
      // we wait again if not in the corner
      if (res.x > 50 || res.y > 50) {
        vm.thread_wait (res, cont);
      }

      // we create the result value
      var b = mk_block (2, 0);
      store_field (b, 0, res.x);
      store_field (b, 1, res.y);

      // we simply use return
      return b;
    }
    vm.thread_wait (res, cont);
  } catch (e) {
    caml_catch(e); // take care of internal exceptions
    this.failwith("corner_click:" + e);
  }
}

function corner_click_callback (cb) {
  var vm = this;
  var res = [];

  try {
    document.getElementById("corner_callback").onclick = function (evt) {
      res.x = evt.clientX;
      res.y = evt.clientY;
      vm.thread_notify_all (res);
    }

    var cont = function  () {
      if (res.x < 50 || res.x > 100 || res.y > 50) {
        vm.thread_wait (res, cont);
      }
      return vm.callback (cb, [res.x, res.y]);
    }
    vm.thread_wait (res, cont);
  } catch (e) {
    caml_catch(e);
    this.failwith("corner_click_callback:" + e);
  }
}
