# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "ubuntu/xenial64"
 
  # Public Network Config:
  # Uncomment the config.vm.network line below to allow other hosts to access this machine via its IP address.
  # Login to the machine and discover its IP address using `ifconfig` so other hosts can access it,
  # This address will look something like http://192.168.1.102:8001 (don't forget the port 8001!) 
  #
  # Note: Vagrant boxes are by design insecure so only do this on a trusted network.
  #       More info: https://www.vagrantup.com/docs/networking/public_network.html
  #       You will also need to choose from your "Available bridged network interfaces"
  #
  # config.vm.network "public_network"

  # Common Settings:
  config.vm.network "forwarded_port", guest: 8001, host: 8001
  # config.ssh.forward_x11 = true
  # config.ssh.forward_agent = true
  config.vm.provider "virtualbox" do |vb|
     vb.customize ["modifyvm", :id, "--memory", "2048"]
     vb.customize ["modifyvm", :id, "--cpus", "2"]
  end

  config.vm.provision "shell", path: "vagrant_scripts/bootstrap_vagrant.sh", privileged: true
  config.vm.provision "shell", path: "vagrant_scripts/bootstrap_app.sh", privileged: false
end
