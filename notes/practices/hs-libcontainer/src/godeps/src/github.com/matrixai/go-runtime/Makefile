.DEFAULT: all

all:
	go build
	
shared:
	go build -buildmode=c-shared -o libcontainer.so *.go
