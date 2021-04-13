ARF Utils Readme
================

The ARF Utils is a website that provides tools for tracking events and
measurements as part of an Applied Rationality Framework implementation.

Server Configuration
--------------------

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

