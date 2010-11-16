#!/usr/bin/env python
hosts = (
    ('192.168.0.78', 'massive'),
    ('192.168.0.189', 'pascal'),
    ('192.168.0.250', 'phoenix'),
    ('192.168.0.253', 'tzara')
    )
with open('/etc/hosts', 'a') as hostfile:
    hostfile.write("\n# automagically appended by ~/.mkhosts.py\n")
    for host in hosts:
        hostfile.write("%s %s\n" % host)
