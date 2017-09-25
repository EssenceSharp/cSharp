# Class ArithmeticValue

superclass: [PrimitiveValue](PrimitiveValue);
instanceArchitecure: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #'arithmetic-double dispatching' method:
[## sumWithSmallInteger: operand

        ^(self coerce: operand) + self
];

protocol: #'arithmetic-double dispatching' method:
[## differenceFromSmallInteger: operand

        ^(self coerce: operand) - self
];

protocol: #'arithmetic-double dispatching' method:
[## productWithSmallInteger: operand

        ^(self coerce: operand) * self
];

protocol: #'arithmetic-double dispatching' method:
[## ratioToSmallInteger: operand

        ^(self coerce: operand) / self
];

protocol: #'arithmetic-double dispatching' method:
[## sumWithFloat: operand

        ^(self coerce: operand) + self
];

protocol: #'arithmetic-double dispatching' method:
[## differenceFromFloat: operand

        ^(self coerce: operand) - self
];

protocol: #'arithmetic-double dispatching' method:
[## productWithFloat: operand

        ^(self coerce: operand) * self
];

protocol: #'arithmetic-double dispatching' method:
[## ratioToFloat: operand

        ^(self coerce: operand) / self
];

protocol: #'arithmetic-double dispatching' method:
[## sumWithDouble: operand

        ^(self coerce: operand) + self
];

protocol: #'arithmetic-double dispatching' method:
[## differenceFromDouble: operand

        ^(self coerce: operand) - self
];

protocol: #'arithmetic-double dispatching' method:
[## productWithDouble: operand

        ^(self coerce: operand) * self
];

protocol: #'arithmetic-double dispatching' method:
[## ratioToDouble: operand

        ^(self coerce: operand) / self
]}}