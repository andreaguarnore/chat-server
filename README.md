A simple TCP/IP chat server in Erlang/OTP25.

The server allows two data "backends": ets and (a local) DynamoDB; the backend is defined in `apps/chat_server/include/backend_macro.hrl`.

## Build
```
$ docker-compose build
```

## Run server
```
$ docker-compose up
```

Note: the hostname of DynamoDB is currently set in the server environment variables such that the server will be able to connect to the database when both are run in Docker; if for any reason you want to test the server locally, you will have to change it to `"localhost"`, instead of `"dynamodb-local"`.

## Create tables
**IMPORTANT**: the script will override your `~/.aws/credentials` file, if you have one.
```
chmod +x aws_config/ddb_config.sh
aws_config/ddb_config.sh
```

## Run client
```
$ make run-client
```

## Client usage
Type `/help` to show the available commands.

