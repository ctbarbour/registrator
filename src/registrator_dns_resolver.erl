-module(registrator_dns_resolver).

-include_lib("dns_erlang/include/dns.hrl").

-export([resolve/1]).

nxdomain(Message) ->
    Message#dns_message{rc = ?DNS_RCODE_NXDOMAIN, aa = true}.

resolve(Message) ->
    resolve(Message, Message#dns_message.questions).

resolve(Message, []) ->
    Message;
resolve(Message, [Question]) ->
    resolve(Message, Question);
resolve(Message, [Question|_]) ->
    resolve(Message, Question);
resolve(Message, Question) when is_record(Question, dns_query) ->
    resolve(Message#dns_message{ra = false, ad = false}, Question#dns_query.name,
	    Question#dns_query.type).

resolve(Message, QName, QType) ->
    Regexp = <<"^_(?<SERVICE>[a-zA-Z]+)\._(?<PROTOCOL>[a-zA-Z]+).(?<DOMAIN>.*)">>,
    case re:run(QName, Regexp, [global, {capture, all, binary}]) of
	{match, [[QName, Service, Protocol, _Domain]]} ->
	    Nodes = registrator_nodes:lookup(Service, Protocol),
	    resolve(Message#dns_message{ra = false, ad = false}, QName, QType, Nodes);
	_ ->
	    nxdomain(Message)
    end.

resolve(Message, _QName, _QType, []) ->
    nxdomain(Message);
resolve(Message, QName, QType, Nodes)
  when QType =:= ?DNS_TYPE_A;
       QType =:= ?DNS_TYPE_AAAA ->
    service_address_records(Message, QName, QType, Nodes);
resolve(Message, QName, QType, Nodes)
  when QType =:= ?DNS_TYPE_SRV ->
    format_srv_records(Message, QName, QType, Nodes);
resolve(Message, QName, QType, Nodes)
  when QType =:= ?DNS_TYPE_ANY ->
    Response = service_address_records(Message, QName, QType, Nodes),
    format_srv_records(Response, QName, QType, Nodes);
resolve(Message, _QType, _QName, _Nodes) ->
    nxdomain(Message).

service_address_records(Message, _QName, _QType, []) ->
    Message#dns_message{rc = ?DNS_RCODE_NXDOMAIN, aa = true};
service_address_records(Message, QName, QType, Nodes) ->
    #dns_message{answers = Answers} = Message,
    Records = lists:filtermap(
		fun(Node) ->
			#{address := Ip} = Node,
			case format_address_record(Ip, QName, QType) of
			    {error, _Reason} ->
				false;
			    Answer ->
				{true, Answer}
			end
		end, Nodes),
    Message#dns_message{answers = Answers ++ Records}.

format_address_record({_, _, _, _} = Ip, QName, QType)
  when QType =:= ?DNS_TYPE_ANY; QType =:= ?DNS_TYPE_A ->
    Data = #dns_rrdata_a{ip = Ip},
    #dns_rr{name = QName, type = ?DNS_TYPE_A, ttl = 0, data = Data};
format_address_record({_, _, _, _, _, _, _, _} = Ip, QName, QType)
  when QType =:= ?DNS_TYPE_ANY; QType =:= ?DNS_TYPE_AAAA ->
    Data = #dns_rrdata_aaaa{ip = Ip},
    #dns_rr{name = QName, type = ?DNS_TYPE_AAAA, ttl = 0, data = Data};
format_address_record(Ip, QName, QType) ->
    {error, {not_found, {Ip, QName, QType}}}.

format_srv_records(Message, _QName, _QType, []) ->
    nxdomain(Message);
format_srv_records(Message, QName, _QType, Nodes) ->
    #dns_message{answers = Answers} = Message,
    SrvRecords = lists:map(
		   fun(Node) ->
			   #{address := Target, port := Port} = Node,
			   Data = #dns_rrdata_srv{priority = 10, weight = 10,
						  port = Port,
						  target = format_ip(Target)},
			   #dns_rr{name = QName, type = ?DNS_TYPE_SRV, ttl = 0, data = Data}
		   end, Nodes),
    Message#dns_message{answers = Answers ++ SrvRecords}.

format_ip(Ip) when is_list(Ip) ->
    list_to_binary(Ip);
format_ip(Ip) when is_binary(Ip) ->
    Ip;
format_ip(Ip) when is_tuple(Ip) ->
    format_ip(inet_parse:ntoa(Ip)).
