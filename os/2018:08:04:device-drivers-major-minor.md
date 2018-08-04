# Device names, major/minor numbers

- Sources: [IBM Explaination](https://www.ibm.com/support/knowledgecenter/en/linuxonibm/com.ibm.linux.z.lgdd/lgdd_c_udev.html), [List of major/minor numbers](https://www.kernel.org/doc/Documentation/admin-guide/devices.txt()

The linux kernel represent character and block devices as pairs of numbers `<major>:<minor>`

- Major numbers can be shared by multiple device drivers
	- Some are reserved for particular device drivers.
	- Some are dynamically assigned when Linux boots.
	- see `/proc/devices` for a list of major numbers and their assigned linux instance.
- Minor numbers are used to distinguish individual physical or logical devices, assigned by the device drivers.
	- E.g. DASD device driver assigns four minor numbers to each DASD: One to the DASD as a whole and other three for up to three partitions.

Userspace programs access character and block devices through *device ndoes* also referred as *device special files*. When a device node is created, it is associated with a major and minor number.


