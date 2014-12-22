---
layout: post
title: "About Not Found"
date: 2014-12-03 15:57:47
categories: ocaml mirage
---
After getting the blog set up initially running in Jekyll things seemed to be moving along smoothly to pushing this thing up to one of the various cloud platforms which might support running a mirage unikernel. The next testing phase was to get this set up to run locally inside the Unix option on my workstation. 

## A Simple Mirage Project 

The mirage project suggests starting with the [mirage-skeleton](https://github.com/mirage/mirage-skeleton) project to build some example unikernels. Basic work in getting the examples built can be found on the [mirage website](http://www.openmirage.org/wiki/hello-world). Mirage leverages the powerful Ocaml module system, specifically [functors](https://realworldocaml.org/v1/en/html/functors.html) to abstract away the fact that you can use different backends. During the build phase environment variables and options can be passed to the `mirage` tool to set up specific backends... at least... I believe that's how it works. 

Ocaml is sufficiently different than the curly brace languages I'm used to and Mirage still seems a bit like black magic right now, so my confidence about that being the case is appropriately limited. Kindly, however, there is a static_website folder from the mirage-skeleton project that contains a basic unikernel for serving html pages from the htdocs folder that can take a complied Jekyll site (really any static site) and serve it. 

After moving some files around and getting the basic project to build without my Jekyll site, it was time to try to get the static blog in that htdocs folder. Jekyll lets you configure a build directory from the _config.yml file, so I opted to let Jekyll do the work for me. I dumped my blog folder inside of a copy of the static_webstie folder and added a `destination: ../htdocs/` to the _config.yml file and ran the `jekyll build` command. 

Next I configured the project to build with the Unix option. 

{% highlight bash %}
$ cd static_website
$ mirage configure --unix
$ make
$ sudo ./mir-www
{% endhighlight %}

Things built without issue and I was ready to test things in my browser. To do that I needed to set up the tuntap device in a separate terminal. 

{% highlight bash %}
$ sudo ifconfig tap0 10.0.0.1 netmask 255.255.255.0 
{% endhighlight %}

Success! Visiting http://10.0.0.2 in Firefox brings up the home page of the blog. The next question, do the posts load? Yes! My first post loads without issue. 

[About Page](/about)...nope. In flat, black letters `Not found` stood irreverently. 

## Groping In The Dark 

What had thus far been smooth sailing over calm seas had taking a turn for the bad. Getting a basic site to be served by mirage was going to be more trial by fire than victory lap. 

Okay, so how to I trouble shoot this? It's not like there's an Apache log spewing out helpful error messages. The console has some output related to ARP routing, but no helpful errors related to http. 

I took a look at the files Jekyll had generated in the htdocs folder. 

{% highlight bash %}
$ ls
about  css  feed.xml  index.html  jekyll
{% endhighlight %}

Interestingly, `about` is a folder and not a file. Which makes sense, I just didn't think of it at the time of the error. Next question was, is there anything in the `about` folder? 

{% highlight bash %} 
$ ls about
index.html
{% endhighlight %}

Yes, index.html. Surely if the index.html is served from the root folder it should be served from any folder, right? 

Next step was to take a look at the Ocaml code. Yes, the black magic. 

## Staring Into The Sun 

A Mirage unikernel is broken down into a couple of different required files. There's one file which is dedicated to the configuration, called  config.ml. This is the file where the magic really happens. At compile time it generates an main.ml file from the various devices you've set up for it to use. They are then passed into the project's root ml file, in this case dispatch.ml at build time and a magical main.ml file is born...black magical. 

With this basic knowledge I took a guess at where the problem was, the dispatch.ml file. The dispatch.ml file only has a few functions and while sparsely documented, it is surprisingly straight forward about what is going on.   

Top of the file opens a few modules which are needed to do things like threading and logging. After that the Main module is parametrised by several modules, the basic console, a file system, and an http server. 

{% highlight ocaml %}
open Lwt
open Printf
open V1_LWT

module Main (C:CONSOLE) (FS:KV_RO) (S:Cohttp_lwt.Server) = struct

  let start c fs http =

...
{% endhighlight %}

The http server, S, serves files stored on the file system. You know, like a normal http server. This part appears to occur inside the `dispatcher` function. A list of strings is checked if it is empty or not. An empty list adds the `index.html` string to a list and calls dispatcher again...so it isn't empty the second time. A non-empty path is assembled into a single string and the server tries to read the file from the file system. If it cannot find the file it returns my irreverent `Not found` error. 

{% highlight ocaml %}
(* dispatch non-file URLs *)
let rec dispatcher = function
  | [] | [""] -> dispatcher ["index.html"] 
  | segments ->
    let path = String.concat "/" segments in
    try_lwt
      read_fs path
      >>= fun body ->
      S.respond_string ~status:`OK ~body ()
    with exn ->
      S.respond_not_found ()
in
{% endhighlight %}

`read_fs path` takes the path and passes it to the `read_fs` function where the file is looked up on the device. This is a [monad](https://www.youtube.com/watch?v=ZhuHCtR3xq8), read_fs returns some string contained in an `Lwt.t`, so the `>>=` operator shoves whatever is returned by that `Lwt.t` into the variable `body` of the next function. Ocaml has labeled arguments (a nice feature for any language) and `~body` is short hand for `~body:body` (in case you were confused by that). 

Couple of things to note about the above code. I'm not sure why this needs to be recursive. Not that recursion is a bad thing in Ocaml, but it just calls dispatcher at most twice with `["index.html"]` in the list the second time. I suspect theres a more graceful way to deal with this (NOTE: my solution is probably not much better for this). Also and more importantly, it only serves the index.html page if and only if it is the root path. That is: it won't serve /about/index.html as /about/ like I want (and I guess Jekyll too). 

So now I need to find a way to change this code so it'll work for Jekyll. This shouldn't be too hard. I just need to find out how to determine if this path is actually a file or a directory. Next stop the [Mirage Documentation](http://mirage.github.io/). 

## Taking A Wrong Turn 

Okay, there's a lot here and I'm not sure what module I'm using here for the file system. It makes sense to stat the file to see if it is a directory or not. I took a look higher up in the code to see what functions were being called on the FS module. 

{% highlight ocaml %}
let read_fs name =
  FS.size fs name
  >>= function
  | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
  | `Ok size ->
     FS.read fs name 0 (Int64.to_int size)
     >>= function
     | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
     | `Ok bufs -> return (Cstruct.copyv bufs)
in
{% endhighlight %}

Okay, so there is a `FS.size` function and a `FS.read` function called here. I could try to see how things are working in the config.ml file. 

{% highlight ocaml %}
...
let mode =
  try match String.lowercase (Unix.getenv "FS") with
    | "fat" -> `Fat
    | _     -> `Crunch
  with Not_found ->
    `Crunch
...
let fs = match mode with
  | `Fat    -> fat_ro "./htdocs"
  | `Crunch -> crunch "./htdocs"
...
let main =
  foreign "Dispatch.Main" (console @-> kv_ro @-> http @-> job)
...
{% endhighlight %}

Yep...black magic. Looks like I'm using [crunch](https://github.com/mirage/ocaml-crunch) and not the fat file system because that's the default. And I don't see a crunch module in the Ocaml documentation...

When faced with a murky coding situation I fall back to my tried and true solution...take a guess. I know it is the file system so something that operates on blocks or has '-fs' at the end of it is a likely candidate. 

After some combing through the documentation, I located what looked like the package I wanted: [Mirage_types.V1:FS.io](http://mirage.github.io/mirage/#Mirage_types.V1:FS.io). It has a `read` function and a `size` function that appears to match what I'm using. Also, it has a `stat` function which is what I need. 

Unfortunately, after modifying the code to use the `stat` function and removing the recursive part of dispatcher I received a compile time error when rebuilding. 

{% highlight bash %}
$ make
...
File "dispatch.ml", line 41, characters 13-20:
Error: Unbound value FS.stat
Command exited with code 2.
make: *** [main.native] Error 10
{% endhighlight %} 

## Out Of The Woods

I'm looking at the wrong module. One of these day's I'll need to get merlin working on my workstation. So I took another poke around the documentation. Actually it is using [Mirage_types.V1:KV_RO](http://mirage.github.io/mirage/#Mirage_types.V1:KV_RO) which makes more sense based on what the main module is using. 

Even with the right module things aren't looking so great. There isn't a `stat` function (actually the error told me that, but the documentation confirmed it). 

Eventually, I settled on a solution which catches the missing file error and then tries again by appending `index.html` to the end of the path. This seems to work well, though I'm not quite satisfied with using exceptions here. 

{% highlight ocaml %}
(* dispatch non-file URLs *)
let dispatcher segments = 
  let path = String.concat "/" segments in 
  try_lwt
    read_fs path 
    >>= fun body -> 
    S.respond_string ~status:`OK ~body () 
    with exn -> 
      try_lwt 
        read_fs (path ^ "/index.html") 
        >>= fun body -> 
        S.respond_string ~status:`OK ~body () 
      with exn -> S.respond_not_found () 
in
{% endhighlight %}

There you have it. Now that [about](/about/) works I'll see about getting this blog up on the cloud. 'Til next time. 
