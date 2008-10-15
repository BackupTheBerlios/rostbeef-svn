import xmlrpclib
server_url = "http://127.0.0.1:4560";
server = xmlrpclib.Server(server_url);
params = {}
params["user"] = "user1"
params["server"] = "localhost"
result = server.get_roster(params)
print result
