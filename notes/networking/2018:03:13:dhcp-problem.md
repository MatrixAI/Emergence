# dhclient

## The Problem
### How I came across the problem
I used `dnsmasq` and `dhclient` to implement my network prototype. However the time for dhclient to discover the dhcp server has always been longer than what I wanted. Turned out it was because of my `dhclient.lease` file. I first noticed that there was something wrong with my lease files by enabling verbose mode when running dhclient. My lease file was corrupted for some unknown reason, so I deleted the files and ran my script again, surprisingly it turned out to be *much quicker*. Just as I thought everything would be fixed if I remove the lease files before running my script, I noticed that my VM no longer have access to the domain name server. So I restarted my virtual machine and noticed that the dhcp lease file contains the information for the domain name server that my VM was originally using, and by removing them at the start of my script, I have changed the DNS to a virtual gateway (that I've created) which does not have a DNS running.

### The actual problem
This is what a DHCP client/server interaction *should* look like, which takes less than 1 second.:
```
DHCPDISCOVER on veth0 to 255.255.255.255 port 67 interval 3 (xid=0x494c7827)
DHCPREQUEST of 10.0.3.78 on veth0 to 255.255.255.255 port 67 (xid=0x27784c49)
DHCPOFFER of 10.0.3.78 from 10.0.3.1
DHCPACK of 10.0.3.78 from 10.0.3.1
```

This is what it looks like after running the script for more than one time, which takes about 10 seconds:
```
DHCPREQUEST of 10.0.3.51 on veth0 to 255.255.255.255 port 67 (xid=0x90c00fa)
DHCPREQUEST of 10.0.3.51 on veth0 to 255.255.255.255 port 67 (xid=0x90c00fa)
DHCPREQUEST of 10.0.3.51 on veth0 to 255.255.255.255 port 67 (xid=0x90c00fa)
DHCPREQUEST of 10.0.3.51 on veth0 to 255.255.255.255 port 67 (xid=0x90c00fa)
DHCPDISCOVER on veth0 to 255.255.255.255 port 67 interval 3 (xid=0xdb55960)
DHCPREQUEST of 10.0.3.128 on veth0 to 255.255.255.255 port 67 (xid=0x6059b50d)
DHCPOFFER of 10.0.3.128 from 10.0.3.1
DHCPACK of 10.0.3.128 from 10.0.3.1
```

The DHCP server tries to request the same IP address as last time, but since the lease is not over yet, the server rejects the DHCPREQUEST and rather than sending back a message, it just wasn't responding. This causes the client to send the DHCPREQUEST message multiple times before sending a DISCOVER to the server. This is what slows the process down. If I take away the last lease everytime we create the container, yes it would speed up next time I request for an address, but the fact that we are occupying a useless spot in the DHCP database is worrying for larger networks. We need a way of either statically assign address to a particular service, or an API to tell the server to release the no longer needed addresses.

To conclude the problem, DHCP Client of a container sends multiple requests using the most recent DHCP lease, which gets rejected by the DHCP server because that address is previously assigned to *another container*. This causes the DHCP address assignment to slow done a lot, which is annoying when dealing with containers since they can be destroyed and created quite often.

## Research Areas
- Understand DHCP protocol and design
- Look into how Docker implements its DHCP
- See if Kea is able to provide an API to control the DHCP server.
