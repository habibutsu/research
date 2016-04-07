TAG="postgres:9.4"
NAME="my-postgres"

CONTAINER_ID=$(shell docker inspect --format="{{.Id}}" ${NAME})
# all:
# 	docker build --tag="postgresql:9.4" .

# run:
# 	docker run --name i-postgres -e POSTGRES_PASSWORD=12345 -d postgres

all:
	docker build --tag="${TAG}" .

create:
	docker create \
		--name ${NAME} \
		--env POSTGRES_PASSWORD=12345 \
		--publish 127.0.0.1:5432:5432 \
		--memory=1024Mb \
		${TAG}

start:
	docker start ${CONTAINER_ID}

stop:
	docker stop bafa32d7d649