Container init processes:

- create.go
  - utils_linux.startcontainer
    - utils_linux.createcontainer
  - utils_lnux.run
    - libcontainer.linux_container.Start()
      - createExecFifo
      - linux_container.start()
        - newParentProcess
          - newSockPair("init")
          - commandTemplate
            - set args = c.InitArgs, which can be found in LinuxFactory


- init.go
  - libcontainer.New("")
  - factory.StartInitialization()
    - os.Clearenv()
    - newContainerInit
      - decodes from pipe
      - populateProcessEnvironment
      - 
