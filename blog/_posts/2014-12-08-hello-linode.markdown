---
layout: post
title: "Hello Linode"
date: 2014-12-08 11:31:44
categories: ocaml mirage linode 
---
It has been almost a week since my last post and I've been working on getting this blog setup on a Xen virtual machine. Most folks seem to be setting up their mirage kernels on Amazon's EC2 instances or their private Xen cloud platforms. I thought it might be nice to try it out on a different public clould/Xen provider. 

### Linode and Mirage 

I haven't been able to find a walk through or tutorial on how to get Mirage set up on Lindoe. Linode provides a number of default Linux distributions which can be installed from a single within the control panel. However, like AWS, they provide a pv-grub instance which can be used to boot a custom distribution. The caveat is that there are quite a few more steps which need to be taken to get your Mirage kernel off the ground. 

The steps to getting this set up were pretty similar to the EC2 tutorial, however I encountered some major issues that were fixed by some minor tweaks of the steps. In the interest of time spent reading this post (also writing it) I won't go too much into what the specific problems were. Suffice to say the menu.lst needs to be a little bit different than what is shown for EC2 on the [Mirage tutorial](http://openmirage.org/wiki/xen-boot). 

Also, I haven't yet worked out how to automate the installation. I'm pretty sure it can work similarly by installing a Linux distribution and then rsyncing in the kernel but I found that I could get the entire Mirage kernel installed on Linode without having to install a separate Linux distribution on the same node and I'd like to keep that as a feature. Again, I haven't gotten the automation worked out yet, but it is coming. 

### Getting things built right

When you build the unikernel you'll have several options on how to build in networking. Linode by default will use DHCP which can make life easy if you compile your Mirage unikernel with DHCP enabled. Like:

{% highlight bash %}
$ env DHCP=true mirage configure --xen
$ make
$ make run  
{% endhighlight %} 

### Basic steps first 

I'm not going to walk through buying an account or setting it up, it is pretty straight forward on how to do. Once you have the account set up head over to the `Linodes` tab and select the 1024 instance type. This is the single core 1024 GB node.

If you already have a Linode you may be dropped into a selection screen for Linodes, just select the one you're going to put mirage on (in this case I've labeled mine 'mirage'). 

![linode-choices](/assets/linode-screenshot-1.png)

You'll otherwise find your way onto the dashboard. From this screen we can set up our initial configuration. First we're going to create our disk image, for this step you'll want to click on the link `Create a new Disk Image`. You'll be provided a place to enter in some disk image information. We won't need a large disk image (remember we won't be installing Linux), so I've elected to do about 1 GB with ext4 and labeled it `mirage-disk`. 

![disk-image](/assets/mirage-disk-image.png)

From here we'll look up to the 'Dashboard' section of the Dashboard (I know :-( look at the image to see what I mean). We're going to click on the `Create a new Configuration Profile` link. 

![dash-board-section](/assets/dashboard-configuration.png)

The next page has a large number of options on it, but there aren't really very many which we need to fill in. Run Level and Memory Limit can be left alone. I like to label things, because later on I'll forget what I was doing and so I'd suggest you also label this configuration. The kernel needs to be pv-grub-x86_64 and we'll assign our single block device, the ext4 partition, to /dev/xvda (not the Finnix iso). Everything else can be left in the default state. Don't forget to save the changes at the bottom.

![configuration-profile](/assets/configuration-profile.png)

### Bowels of the Beast 

So far we've just been manipulating the GUI to get the configuration that we want. Unfortunately, we're going to need to do some command line work to put this thing into action. 

In order to get the image onto the server we're going to rsync it to our disk image. To do that we would ordinarily need either access to the host or an environment which supported the other end of rsync and ssh (like another Linux distribution). 

As stated before, we're not going to install another Linux distribution here. Instead Linode provides a rescue environment they call lish and a rescue image called Finnix. To boot to Finnix we need to click on the `Rescue` tab and click on `Reboot into Rescue Mode`. 

![rescue-layer](/assets/rescue-layer.png)

Once we click on `Reboot into Rescue Mode`, we'll be dropped back into the dashboard and the instance will begin starting up into rescue mode. Next we'll need to click on the `Remote Access` tab. 

This tab has some information on how to access the system remotely. In this instance we want to take a look toward the bottom of the page under the `Console Access` section where it says `Lish via ssh`. 

It will contain a line detailing how to ssh into the rescue layer (really lish, but since we've started Finnix we'll be dropped into that). Copy and paste this line into your terminal. You'll be prompted for a password which will be the account password for Linode. 

From here there are only a few more steps we need to take to get our Mirage kernel working. We're going to need to mount the /dev/xvda disk image and add a few directories and files to it.

{% highlight bash %} 
$ mount /dev/xvda /media/
$ cd /media/ 
$ mkdir -p boot/grub
{% endhighlight %}

After making the boot and grub directories on /dev/xvda, we can then create the boot/grub/menu.lst file. It should contain text similar to the following (NOTE: your mirage kernel may not have the same name but should generally end with .xen). You'll need to use nano or cat fu for this as that's what is installed on Finnix. You could also rsync this file over using similar instructions to getting the kernel on the server. 

{% highlight bash %}
$ cat > /media/boot/grub/menu.lst << EOF
> timeout 1
>
> title Mirage
> root (hd0)
> kernel /boot/mir-www.xen root=/dev/xvda ro quiet
> EOF
{% endhighlight %}
After this is done we'll need to set up a root password and start ssh so we can get the Mirage kernel onto the Linode. 

{% highlight bash %}
$ passwd
Enter new UNIX password: 
Retype new UNIX password: 
passwd: password updated successfully 
$ service ssh start
{% endhighlight %}

Once ssh has started we can then rsync the kernel over ssh from our local work station. 

{% highlight bash %}
$ rsync -avPz mir-www.xen root@ip.address.ofyour.linode:/media/boot/
{% endhighlight %}

### It's Alive!

After this is complete, go back to your Linode Dashboard (under the dashboard tab) and click `Reboot`. If all goes well you should be able to navigate to your Linode's IP in a browser and see it serving a page. (NOTE: Linode recommends a swap partition and not having one will prompt a warring on the Dashboard. Since we aren't using Linux I don't believe that is too much of a concern.) 

At this point you know as much as I do about how to get started with Mirage on Linode. I'm still learning a bit about Jekyll and getting images working was my lesson for the day. I'll see about getting some sort of comment system up as I'm sure there may be some questions about how to do this. 
