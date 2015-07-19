---
title: Using Loggly On Elastic Beanstalk
author: Philip Cunningham
tags: ops
---

``` yaml
# .ebextensions/loggly.config
files:
  "/tmp/loggly_configure.sh":
    mode: "000755"
    owner: root
    group: root
    content: |
      #!/bin/bash
      su --command="python /tmp/loggly.py setup --auth $LOGGLY_AUTH --account $LOGGLY_ACCOUNT --yes"

container_commands:
  01_loggly_dl:
    command:
      wget -q -O /tmp/loggly.py https://www.loggly.com/install/configure-syslog.py
  02_loggly_config:
    command: /tmp/loggly_configure.sh
```
