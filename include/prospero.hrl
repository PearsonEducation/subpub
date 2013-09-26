% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-record(pe_principal, {id, friendly_name, enforced_tag_ids=[], is_require_message_type_with_new_subs=false, date_created, date_deactivated, secret, realm="*", delivery_url_mask="*"}).

-record(pe_sub, {id, principal_id, callback_url, wsdl_uri, queue_name, tag_ids=[], date_created, date_cancelled, duplication_key}).

-record(pe_tag, {id, type, value}). %% Note:  Changing fields here means you have to also update the object queries in pe_tag_store.erl.

%-record(pe_msg, {id, principal_id, status, payload_content_type, payload_version, message_type, date_received, date_cancelled, date_expires, tag_ids}).

%-record(pe_del_order, {id, msg_id, sub_id, date_received, status, date_delivered, num_attempts}).

%-record(pe_del_attempt, {id, del_order_id, date_attempted, date_completed, status, error_desc, consumer_ref_num}).

-record(pe_amqp_conn, {connection, exchange, channel, host}).

-record(pe_kvpair, {key, value}).

-record(status, {node, version, num_messages_posted, num_messages_delivered, num_failed_deliveries, time_started, uptime, num_watched_subscriptions, subscription_pids, pids_awaiting_broker_reconnection=[], rest_status}).

-define(PROSPERO_VERSION, r3).
-define(CLIENT_TAG_TYPE, "Client").
-define(CLIENT_STRING_TAG_TYPE, "ClientString").
-define(MESSAGE_TYPE_TAG_TYPE, "MessageType").
-define(SYSTEM_TAG_TYPE, "System").
-define(SUB_SYSTEM_TAG_TYPE, "SubSystem").
