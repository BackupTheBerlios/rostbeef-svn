import xmlrpclib
server_url = "http://127.0.0.1:4560";
server = xmlrpclib.Server(server_url);
params = {}
params["user"] = "user1"
params["server"] = "localhost"
params["jid"] = "user2@localhost"
params["group"] = []
params["nick"] = "test"
params["subs"] = "both"
result = server.add_rosteritem(params)
print result
