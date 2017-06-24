# Prerequisites

SPSC web application generates partial process trees in [SVG 
format](http://www.w3.org/Graphics/SVG/). You need to have SVG-compatible 
web-browser. Mozilla Firefox and Opera already support SVG. Internet Explorer 
doesn't provide support for SVG. In order to view SVG images with IE you
need to install
[Adobe SVG Viewer plug in](http://www.adobe.com/svg/viewer/install/main.html).

# Usage

SPSC web application is hosted at <http://pat.keldysh.ru:8180/spsc_web/spsc>
and already has some predefined samples:

![http://wiki.spsc.googlecode.com/hg/images/webinput.png](http://wiki.spsc.googlecode.com/hg/images/webinput.png)

Start playing with samples. SPSC produces residual program and partial process 
tree:

![http://wiki.spsc.googlecode.com/hg/images/weboutput.png](http://wiki.spsc.googlecode.com/hg/images/weboutput.png)

Also SPSC takes care about input correctness:

![http://wiki.spsc.googlecode.com/hg/images/websyntax.png](http://wiki.spsc.googlecode.com/hg/images/websyntax.png)

# Advanced: Hosting SPSC web demonstration

SPSC web demonstration is powered by
[Scala Lift Web Framework](http://liftweb.net) and is distributed as Java
Servlet web application packaged in ready-for-hot-deployment
[war file](http://en.wikipedia.org/wiki/Sun_WAR_(file_format) ).

Install servlet container with
[Java Servlet](http://java.sun.com/products/servlets) and
[JavaServer Pages](http://java.sun.com/products/jsp) support.
SPSC requires support for Servlet v2.4 and JSP v2.0.
[Apache Tomcat v5.5](http://tomcat.apache.org/) is a good choice. Download
`spcs_web_beta.zip` from Downloads section. This archive contains just one file
`spsc_web.war`. Place `spsc_web.war` in Tomcat `webapps` directory. That's all!
Start Tomcat. SPSC will be available at
`http://localhost:{tomcat_port}/spsc_web`.

`{tomcat_port}` is 8080 by default but may vary depending on a way you install Tomcat.
