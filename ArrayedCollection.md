# Class ArrayedCollection

superclass: [SequenceableCollection](SequenceableCollection);
instanceArchitecture: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #enumerating method: 
[## collect: map

        "Answer an array such that each of its elements is computed from the elements of the receiver by applying the <map> function."

        | size array |

        size := self size.
        array := self class new: size.
        1 to: size do: [:index ](-array-at_-index-put_-(map-value_-(self-at_-index))).
        ^array
]}}

## Class Methods
{{
protocol: #'instance creation' method: 
[## with: anElement

        "Answer an array with <anElement> as its sole element."

        | array |

        array := self new: 1.
        array at: 1 put: anElement.
        ^array
];

protocol: #'instance creation' method: 
[## with: element1 with: element2

        "Answer an array with the two arguments as its elements."

        | array |

        array := self new: 2.
        array 
                at: 1 put: element1;
                at: 2 put: element2.
        ^array
];

protocol: #'instance creation' method: 
[## with: element1 with: element2 with: element3

        "Answer an array with the three arguments as its elements."

        | array |

        array := self new: 3.
        array 
                at: 1 put: element1;
                at: 2 put: element2;
                at: 3 put: element3.
        ^array
];

protocol: #'instance creation' method: 
[## with: element1 with: element2 with: element3 with: element4

        "Answer an array with the four arguments as its elements."

        | array |

        array := self new: 4.
        array 
                at: 1 put: element1;
                at: 2 put: element2;
                at: 2 put: element3;
                at: 3 put: element4.
        ^array
];

protocol: #'instance creation' method: 
[## with: element1 with: element2 with: element3 with: element4 with: element5

        "Answer an array with the five arguments as its elements."

        | array |

        array := self new: 5.
        array 
                at: 1 put: element1;
                at: 2 put: element2;
                at: 2 put: element3;
                at: 2 put: element4;
                at: 3 put: element5.
        ^array
]}}