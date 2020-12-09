# TLS configuration

It is possible to setup the application to use TLS. You need to add the following
configuration to the `web-server` configuration section:

```
    tls {
        keyStore = "<path to java keystore>"
        keyStorePwd = "<keystorepass>"
        certPwd = "<certpass>"
    }
```

On production these settings shouldn't be on github. Instead they are stored
in the server and referred from the configurationFile

# Keystore creation

The application needs the certificates on a java keystore and setting it properly
is something of a black art. Here is the procedure to set it up using the certificates
provided by ITS.

Those certificates have the following chain:

```
gn-ca0
  |
  |- GN-CA1
       |
       |- site certificate
```

From ITS we get two files in PEM format the cert: `site.cert.pem` and the private key `site.key.pem`.
Additionally there is the intermediate certificate in a file `GN-CA1.pem`

Now we are ready to build the keystore as follows:

## Per host passwords

The keystore and certificate passwords are stored on each target host at

```
/gemsoft/etc/seqexec/conf.d/tls.conf
```

## Concatenate the intermediate and the site certificates

```
cat gn-ca0.pem GN-CA1.pem site.cert.pem > site.cert.pem
```

## Make a PKCS12 keystore

Due to some limitations on java's keystore handling of private keys we need to do this with `openssl`

```
openssl pkcs12 -export -in site.cert.pem -inkey site.key.pem -out site.p12 -name <site-name> -password pass:<certpass>
```

This will give you a pkcs12 store with the certificate and key

## Construct a java keystore from the pkcs12

** Note:** Delete any existing keystore before importing

```
keytool -importkeystore -deststorepass <keystorepass> -destkeypass <certpass> -destkeystore site.jks -srckeystore site.p12 -srcstoretype PKCS12 -srcstorepass <certpass> -alias seqexec-gs-test
```

## Verify that the keystore contains one certificate

```
keytool -list -keystore site.jks -storepass <keystorepass>

Keystore type: JKS
Keystore provider: SUN

Your keystore contains 1 entry

<site-name>, Apr 26, 2017, PrivateKeyEntry,
Certificate fingerprint (SHA1): 41:57:28:8A:6A:CE:F8:C8:4F:10:DC:C2:33:93:E0:70:69:47:3A:51
```

## Copy the keystore and the configuration file to the destination server

# Verification

You can verify that the certificate chain is correct by using the openssl `s_client`

```
openssl s_client -connect <host>:<port> -showcerts
```

Note that in OSX openssl `s_client` could fail to validate the certificate since it doesn't use the system root certPwd

See [here](http://jw35.blogspot.cl/2011/02/root-certificates-for-macos-openssl.html) for a workaround

# Root certificate

Internal sites are provided with a certificate signed by Gemini's root CA which is not necessarily installed
in all systems. In particular Firefox has its own CA store and the certificate should be installed manually there

The code here cannot solve this at the moment
