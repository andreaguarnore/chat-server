CLIENT_PROJ = chat_client

compile:
	@./rebar3 compile

run-server:
	@./rebar3 shell

run-client: compile
	@erl -pa "_build/default/lib/$(CLIENT_PROJ)/ebin/" -eval "application:start($(CLIENT_PROJ))" -noshell -s init stop

clean:
	@./rebar3 clean

rebar:
	@wget -q https://s3.amazonaws.com/rebar3/rebar3
	@chmod +x rebar3

