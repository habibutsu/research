IMAGE="mycentos:7"
CONTAINER="CentOS7"

CONTAINER_ID=$(shell docker inspect --format="{{.Id}}" ${CONTAINER})
CONTAINER_PID=$(shell docker inspect -f '{{ .State.Pid }}' ${CONTAINER})
CONTAINER_FS_ID=$(shell docker exec ${CONTAINER} sh -c "mount|head -1|awk '{print \$$1}'")

all:
	docker build --tag="mycentos:7" .

run:
	docker run -it --rm --name ${CONTAINER} \
		--cidfile=run.cid --memory=1024Mb ${IMAGE} bash

io-test:
	docker exec -it ${CONTAINER} \
		sh -c "time (dd if=/dev/zero of=testfile bs=1M count=100 && sync); \
		rm testfile"

limit-fs:
	systemctl set-property --runtime \
		docker-${CONTAINER_ID}.scope "BlockIOWriteBandwidth=${CONTAINER_FS_ID} 100M"

info:
	@#systemctl show docker-${CONTAINER_ID}
	@echo "NAME: ${CONTAINER}"
	@echo "ID: ${CONTAINER_ID}"
	@echo "PID: ${CONTAINER_PID}"

# https://goldmann.pl/blog/2014/09/11/resource-management-in-docker/#_limiting_read_write_speed
# https://coelhorjc.wordpress.com/2015/03/24/using-docker-linux-containers-containers-images-machine-compose-swarm-weave/
# https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/7/html-single/Resource_Management_Guide/