.PHONY: all libcontainer clean
.DEFAULT: libcontainer

SOURCES := *.go

libcontainer: $(SOURCES)
	go build -buildmode=c-shared -o libcontainer.so $(SOURCES)

clean:
	go clean
