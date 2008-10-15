import xmlrpclib
server_url = "http://127.0.0.1:4560";
server = xmlrpclib.Server(server_url);
params = {}
result = server.echothis("Hello World")
print result
