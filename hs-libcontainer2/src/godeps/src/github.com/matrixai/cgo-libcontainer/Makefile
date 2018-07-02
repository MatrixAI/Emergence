.PHONY: all libcontainer clean
.DEFAULT: libcontainer

libcontainer: $(SOURCES)
	go build -buildmode=c-shared -linkshared -o libcontainer.so $(SOURCES)

clean:
	go clean
