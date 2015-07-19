 ---
title: Using Loggly On Elastic Beanstalk
author: Philip Cunningham
tags: ops
---

This week I had to configure a Rails app running on Elastic Beanstalk
to use Loggly. Since I don't want anyone to suffer the Elastic
Beanstalk documentation more than is necessary, I've written this
short tutorial should you find yourself tasked with the same
challenge. It assumes you already have a Rails app running on Elastic
Beanstalk and a general understanding of Rails apps.

## Configure App

Add the [syslogger](https://github.com/crohr/syslogger) gem to your Gemfile.

``` ruby
# Gemfile
gem 'syslogger', '~> 1.6.0'
```

In `config/environments/production.rb` configure your app to use
`Syslogger` for logging. Replace `"your-app-name"` with, you guessed
it, the name of your application. This allows you to easily see logs
from your app in the Loggly web interface using
[source groups](https://www.loggly.com/docs/source-groups/), which is
especially useful if you're using Loggly to aggregate logs from
multiple apps.

``` ruby
# config/environments/production.rb
config.logger = Syslogger.new("your-app-name", Syslog::LOG_PID, Syslog::LOG_LOCAL7)
```

## Create Loggly Account

Go [sign up](https://www.loggly.com/signup/) for a
[Loggly](https://www.loggly.com/) account and
[create a customer token](https://www.loggly.com/docs/customer-token-authentication-token/).

## Configure Container Environment

Go to the Amazon AWS Console and add your Loggly account name and
customer token to your Elastic Beanstalk app's environment using the
`LOGGLY_ACCOUNT` and `LOGGLY_AUTH` property names.

## Customise Container

Add the following configuration to `loggly.config` in the `.ebextensions`
directory in the root of your app.

``` yaml
# .ebextensions/loggly.config
files:
  "/tmp/loggly_config.sh":
    mode: "000755"
    owner: root
    group: root
    content: |
      #!/bin/bash
      su --command="python /tmp/configure-syslog.py setup --auth $LOGGLY_AUTH --account $LOGGLY_ACCOUNT --yes"

container_commands:
  01_loggly_dl:
    command:
      wget -q -O /tmp/configure-syslog.py https://www.loggly.com/install/configure-syslog.py
  02_loggly_config:
    command: /tmp/loggly_config.sh
```

This YAML file downloads Loggly's own syslog configuration script and
sets up your container to send your system logs to Loggly. You can
audit this script at
[loggly/install-script](https://github.com/loggly/install-script/blob/master/configure-syslog.py).

## Wrap Up

Re-deploy your app to Elastic Beanstalk and watch as your logs begin
appearing in Loggly.
