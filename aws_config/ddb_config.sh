#!/bin/bash

# configure local
echo -ne "mykey\nmysecret\neu-south-1\njson" | aws configure

# create tables
port=8000
aws dynamodb create-table \
  --table-name Users \
  --attribute-definitions \
     AttributeName=Name,AttributeType=S \
  --key-schema \
     AttributeName=Name,KeyType=HASH \
  --provisioned-throughput \
     ReadCapacityUnits=5,WriteCapacityUnits=5 \
  --table-class STANDARD \
  --endpoint-url http://localhost:$port
aws dynamodb create-table \
  --table-name Rooms \
  --attribute-definitions \
    AttributeName=Name,AttributeType=S \
  --key-schema \
    AttributeName=Name,KeyType=HASH \
  --provisioned-throughput \
     ReadCapacityUnits=5,WriteCapacityUnits=5 \
  --table-class STANDARD \
  --endpoint-url http://localhost:$port

