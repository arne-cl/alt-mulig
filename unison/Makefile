build: clean
	docker build -t unison-2.51.2 -f Dockerfile-build-unison .
	docker create -ti --name dummy unison-2.51.2 bash
	docker cp dummy:/opt/unison-2.51.2/src/unison unison
	docker rm -f dummy

install: build
	cp unison /usr/local/bin/unison

clean:
	-rm unison
	docker rm -f dummy


