# Raising a CLR Event

To raise a CLR event, it is necessary to define an **invokeEvent** _user primitive_ for that specific event--as explained in the section on [using CLR types](using-CLR-types)--and then send whatever message will invoke that primitive to an object whose Essence# class defines or inherits that primitive.

Here's an example of the method declaration for an **invokeEvent** primitive, which will invoke a CLR event named **tick:**

{{
invokeTickEvent: eventSource args: eventArgs

        <invokeEvent: #tick>        
}}

# Registering Event Handlers
To subscribe to a .Net event, send the message **#onEvent: _eventName_ do: _aBlockOrDelegate_** to the object that defines the event (the receiver must be a native CLR object, not an Essence# object.)

To unsubscribe from a .Net event, send the message **#onEvent: _eventName_ doNotDo: _aBlockOrDelegate_** to the object that defines the event (the receiver must be a native CLR object, not an Essence# object.)

# Integration of .Net Events With Essence# Announcments
In order to provide a bridge bewteen .Net events and Essence# Announcements,  the Essence# class CLR.System.Object provides the following methods:

{{
protocol: #'events-subscribing' method:

[## when: anAnnouncementClass do: anAction
	
	^self onEvent: anAnnouncementClass eventName do: anAction
];
	
protocol: #'events-subscribing' method:

[## when: anAnnouncementClass doNotDo: anAction
	
	^self onEvent: anAnnouncementClass eventName doNotDo: anAction
]
}}
_
The essence of OOP: It's all messages, all the time._
