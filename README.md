## EMQ-POLICY-SERVER
policy server for EMQ

## Get Started
* Clone ` emq-relx ` project
```
git clone https://github.com/emqtt/emq-relx.git
cd emq-relx
```
* In Makefile,
```
DEPS += emq_policy_server
dep_emq_policy_server = git https://github.com/whoopschat/emq-policy-server master
```
* In relx.config
```
{emq_policy_server, load}
```
* In etc/acl.conf 
```
{deny, all, subscribe, ["#", {eq, "#"}]}.
```
* Build
```
make
```
## Author
* [WhoopsChat](https://github.com/whoopschat)

