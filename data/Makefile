PROJECT = emq-relx
PROJECT_DESCRIPTION = Release Project for the EMQ Broker
PROJECT_VERSION = 2.3.5

## Fix 'rebar command not found'
DEPS = goldrush
dep_goldrush = git https://github.com/basho/goldrush 0.1.9

DEPS += emqttd emq_dashboard emq_retainer emq_recon emq_reloader \
        emq_sn emq_coap emq_stomp \
        emq_lwm2m

# whoopschat policy
DEPS += emq_policy_server
dep_emq_policy_server = git https://github.com/whoopschat/emq-policy-server master

# emq deps
dep_emqttd        = git https://github.com/emqtt/emqttd master
dep_emq_dashboard = git https://github.com/emqtt/emq-dashboard master
dep_emq_retainer  = git https://github.com/emqtt/emq-retainer master
dep_emq_recon     = git https://github.com/emqtt/emq-recon master
dep_emq_reloader  = git https://github.com/emqtt/emq-reloader master

# mqtt-sn, coap and stomp
dep_emq_sn    = git https://github.com/emqtt/emq-sn master
dep_emq_coap  = git https://github.com/emqtt/emq-coap master
dep_emq_stomp = git https://github.com/emqtt/emq-stomp master
dep_emq_lwm2m = git https://github.com/emqtt/emq-lwm2m master

# COVER = true

include erlang.mk

plugins:
	@rm -rf rel
	@mkdir -p rel/conf/plugins/ rel/schema/
	@for conf in $(DEPS_DIR)/*/etc/*.conf* ; do \
		if [ "emq.conf" = "$${conf##*/}" ] ; then \
			cp $${conf} rel/conf/ ; \
		elif [ "acl.conf" = "$${conf##*/}" ] ; then \
			cp $${conf} rel/conf/ ; \
		else \
			cp $${conf} rel/conf/plugins ; \
		fi ; \
	done
	@for schema in $(DEPS_DIR)/*/priv/*.schema ; do \
		cp $${schema} rel/schema/ ; \
	done

app:: plugins
