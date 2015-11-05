post:
	curl --request POST --data "{\"entry\":{\"active\":true,\"to\":\"jim\",\"from\":\"sofia\",\"amount\":3.14}}" localhost:8000/entries

get:
	curl --request GET localhost:8000/entries
