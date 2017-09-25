# Class ByteArray

superclass: [ArrayedCollection](ArrayedCollection);
instanceArchitecture: #IndexedByteSlots

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #'system primitives' method:
	[## at: slotIndex 

		<primitive: #at: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## do: enumerator 

		<primitive: #do: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## from: startIndex to: endIndex do: enumerator 

		<primitive: #from:to:do: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## from: startIndex to: endIndex by: step do: enumerator 

		<primitive: #from:to:by:do: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## at: slotIndex put: newValue 

		<primitive: #at:put: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## nextIdentityIndexOf: element ifAbsent: startIndex 

		<primitive: #nextIdentityIndexOf:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## prevIdentityIndexOf: element ifAbsent: startIndex 

		<primitive: #prevIdentityIndexOf:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## identityIncludes: element 

		<primitive: #identityIncludes: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## add: suffix 

		<primitive: #add: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyWith: suffix 

		<primitive: #copyWith: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## identityRemoveNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #identityRemoveNext:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyIdentityRemovingNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyIdentityRemovingNext:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## identityRemovePrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #identityRemovePrev:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyIdentityRemovingPrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyIdentityRemovingPrev:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## identityRemoveAll: removal 

		<primitive: #identityRemoveAll: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyIdentityRemovingAll: removal 

		<primitive: #copyIdentityRemovingAll: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## insert: infix at: insertionIndex 

		<primitive: #insert:at: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyInserting: infix at: insertionIndex 

		<primitive: #copyInserting:at: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## nextIndexOf: element startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #nextIndexOf:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## prevIndexOf: element startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #prevIndexOf:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## includes: element 

		<primitive: #includes: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## removeNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #removeNext:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyRemovingNext:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## removePrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #removePrev:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingPrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyRemovingPrev:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## removeAll: removal 

		<primitive: #removeAll: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingAll: removal 

		<primitive: #copyRemovingAll: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## atFirstPutOrAdd: newValue 

		<primitive: #atFirstPutOrAdd: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## atLastPutOrAdd: newValue 

		<primitive: #atLastPutOrAdd: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## setSize: newSize 

		<primitive: #setSize: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyWithSize: newSize 

		<primitive: #copyWithSize: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## firstIfNone: noSuchElementAction 

		<primitive: #firstIfNone: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## lastIfNone: noSuchElementAction 

		<primitive: #lastIfNone: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## nextIndexSuchThat: predicate startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #nextIndexSuchThat:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## prevIndexSuchThat: predicate startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #prevIndexSuchThat:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## contains: predicate 

		<primitive: #contains: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## addAll: suffix 

		<primitive: #addAll: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## , suffix

		<primitive: #, domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyFrom: startIndex to: endIndex 

		<primitive: #copyFrom:to: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyTo: endIndex 

		<primitive: #copyTo: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyFrom: startIndex 

		<primitive: #copyFrom: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## withAllButFirst

		<primitive: withAllButFirst domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## withAllButLast

		<primitive: withAllButLast domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## withFirst

		<primitive: withFirst domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## withLast

		<primitive: withLast domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## removeAt: index 

		<primitive: #removeAt: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingAt: index 

		<primitive: #copyRemovingAt: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyAndRemoveFrom: startIndex to: endIndex 

		<primitive: #copyAndRemoveFrom:to: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingFrom: startIndex to: endIndex 

		<primitive: #copyRemovingFrom:to: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## removeNextSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #removeNextSuchThat:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingNextSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #copyRemovingNextSuchThat:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## removePrevSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #removePrevSuchThat:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingPrevSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #copyRemovingPrevSuchThat:startingAt:ifAbsent: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## removeAllSuchThat: predicate 

		<primitive: #removeAllSuchThat: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingAllSuchThat: predicate 

		<primitive: #copyRemovingAllSuchThat: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## insertAll: infix at: insertionIndex 

		<primitive: #insertAll:at: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyInsertingAll: infix at: insertionIndex 

		<primitive: #copyInsertingAll:at: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## moveFrom: startIndex to: stopIndex by: displacement 

		<primitive: #moveFrom:to:by: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyMovingFrom: startIndex to: stopIndex by: displacement 

		<primitive: #copyMovingFrom:to:by: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyReplacingFrom: startIndex to: stopIndex with: replacementSource startingAt: replacementSourceStartIndex 

		<primitive: #copyReplacingFrom:to:with:startingAt: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## reverse

		<primitive: reverse domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyReversed

		<primitive: copyReversed domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## reverseFrom: startIndex to: endIndex 

		<primitive: #reverseFrom:to: domain: IndexedByteSlots>
	];

	protocol: #'system primitives' method:
	[## copyReversedFrom: startIndex to: endIndex 

		<primitive: #copyReversedFrom:to: domain: IndexedByteSlots>
	]}}
