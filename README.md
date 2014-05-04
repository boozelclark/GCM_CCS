GCM_CCS
========

A GCM (Google Cloud Messageing) CCS (Cloud Connection Server) written in Erlang that supports upstream and downstream messaging



This application requires eXMPP to work correctly. This can be found at http://exmpp.org

Your google Cloud Messaging creditials need to be configured in the file /src/gcm_ccs.app.src Set your GCM credentials as the defined environment variables.

To run the application call ```application:start(gcm_ccs).```.

##Upstream Messaging
You can attach a gen_event message handler to receive upstream messages using gcm:attach_handler/2 which takes the handler module and a name as parameters.
A sample handler has been included in the application and can be added using the command ```gcm:attach_handler(sample_handler,sample).``` and removed using ```gcm:remove_handler(sample_handler,sample).```.

##Downstream Messaging
Down strem messages can be sent using gcm:send/3 whihc takes the registraion id, message and message id as list parameters.
