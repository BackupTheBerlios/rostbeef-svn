import xmlrpclib
server_url = "http://127.0.0.1:4560";
server = xmlrpclib.Server(server_url);
params = {}
params["jid1"] = "user1@localhost"
params["nick1"] = "user1nick"
params["jid2"] = "user2@localhost"
params["nick2"] = "user2nick"
result = server.link_contacs(params)
print result
