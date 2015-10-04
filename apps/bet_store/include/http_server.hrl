% http rest definitions
-define(HTTP_METHOD_GET,    "GET").
-define(HTTP_METHOD_POST,   "POST").
-define(HTTP_METHOD_PUT,    "PUT").
-define(HTTP_METHOD_DELETE, "DELETE").
-define(HTTP_METHOD_HEAD,   "HEAD").

% http requests definitions
-define(HTTP_REQUEST_STORE, "store").

-record(http_response, {
                        code, 
                        content_type, 
                        content_length=null, 
                        content
}).

-record(http_server_config, {
                             port,
                             name,
                             root_dir,
                             document_dir,
                             log_error,
                             log_transfer,
                             log_security
}).


-define(HTTP_STATUS_OK, 200).
-define(HTTP_STATUS_CREATED, 201).
-define(HTTP_STATUS_BAD_REQUEST, 400).
-define(HTTP_STATUS_NOT_FOUND, 404).
-define(HTTP_STATUS_NOT_ALLOWED, 405).
-define(HTTP_STATUS_CONFLICT, 409).
