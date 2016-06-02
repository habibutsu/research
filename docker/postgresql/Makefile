NAME="postgres_db"

start:
	@echo "=> Start container"
	@if [[ "$(shell docker ps -q -f name=postgres_db 2> /dev/null)" == "" ]]; then \
		docker start postgres_db; \
	else \
		echo "Container already started"; \
	fi

container:
	@echo "=> Creation of container"
	@if [[ "$(shell docker ps -a -q -f name=postgres_db 2> /dev/null)" == "" ]]; then \
		docker create \
			--name postgres_db \
			--tty --interactive \
			--env POSTGRES_USER=postgres \
			--env POSTGRES_DB=postgres \
			--env POSTGRES_PASSWORD=dbpassword \
			--publish=5432:5432 \
			postgres:9.5; \
	else \
		echo "Container already exist"; \
	fi

# make adddb DBUSER=dbuser DBNAME=testdb
adddb: start
	docker exec -ti postgres_db /bin/bash -c "\
		export PGPASSWORD=$$POSTGRES_PASSWORD; \
		psql --username=\"\$$POSTGRES_USER\" \
			-c \"CREATE USER ${DBUSER}; CREATE DATABASE ${DBNAME} OWNER ${DBUSER}\""

console: start
	docker exec -ti postgres_db /bin/bash -c '\
		export PGPASSWORD=$$POSTGRES_PASSWORD; \
		psql --host="$${HOSTNAME}" --username="$$POSTGRES_USER" $$POSTGRES_DB'

shell: start
	docker exec -ti postgres_db bash