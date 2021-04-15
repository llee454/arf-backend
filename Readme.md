ARF Utils Readme
================

The ARF Utils is a website that provides tools for tracking events and
measurements as part of an Applied Rationality Framework implementation.

Server Configuration
--------------------

### Enable CGI Scripts

### Rewrite Rules

The backend assumes that you have configured the Apache HTTPD Rewrite module to
rewrite requests as follows:

```
<Directory /var/www/cgi-bin>
  RewriteEngine On
  RewriteBase / 
  RewriteCond %{REQUEST_FILENAME} !-f
  RewriteCond %{REQUEST_FILENAME} !-d
  RewriteRule ^(.*)$ arf.cgi?q=$1 [L,QSA]
</Directory>
```

Testing
-------

You can test the backend by sending requests to the CGI script via the command
line. For example, the following sends a request asking the backend script to
create a new event:

```
$ env QUERY_STRING='q=event/create' ./arf.cgi <<EOF
{"key": null, "created": 123456, "timestamp": 123456}
EOF
```

The following command asks the backend to read the first event in the database.

```
$ env QUERY_STRING='q=event/read/1' ./arf.cgi <<EOF
EOF
```
