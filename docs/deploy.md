Deployment
============================


The App runs on Thunderbolts, a dedicated machine configured by Tony running in the data center


Connecting to Thunderbolts
--------------------------

First use Cisco to connect to the VPN: https://oit.colorado.edu/services/network-internet-services/vpn/help/cisco-vpn

Once connected, SSH into Thunderbolts, and restore the tmux session

    > shess@10.224.180.34
    > tmux attach || tmux new

Application Running in TMUX
---------------------------

For now, the app runs in a long-running tmux session

    > cd level2
    > cabal run

NGINX
-------------

Configure NGINX

    > sudo su
    > nvim /etc/nginx/sites-available/thunderbolts.dev.dkistdc.nso.edu

Restart NGINX

    >  sudo systemctl restart nginx


Visit Site
-----------

https://thunderbolts.dev.dkistdc.nso.edu/
