-record(wok_resp, {
          code = 200
          , headers = []
          , body = <<>>
         }).

-record(wok_req_params, {
          tmp_dir = undefined,
          post = [],
          get = [],
          bind = [],
          files = []}).

-record(wok_req, {
          request = undefined
          , custom_data = #{}
          , global_state = undefined
          , local_state = undefined
          , handler = undefined
          , adapter = undefined
          , response = #wok_resp{}
          , params = #wok_req_params{}
         }).


