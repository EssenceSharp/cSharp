# Class HalfWordArray

superclass: [ArrayedCollection](ArrayedCollection);
instanceArchitecture: #IndexedHalfWordSlots

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #'system primitives' method:
	[## at: slotIndex 

		<primitive: #at: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## do: enumerator 

		<primitive: #do: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## from: startIndex to: endIndex do: enumerator 

		<primitive: #from:to:do: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## from: startIndex to: endIndex by: step do: enumerator 

		<primitive: #from:to:by:do: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## at: slotIndex put: newValue 

		<primitive: #at:put: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## nextIdentityIndexOf: element ifAbsent: startIndex 

		<primitive: #nextIdentityIndexOf:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## prevIdentityIndexOf: element ifAbsent: startIndex 

		<primitive: #prevIdentityIndexOf:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## identityIncludes: element 

		<primitive: #identityIncludes: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## add: suffix 

		<primitive: #add: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyWith: suffix 

		<primitive: #copyWith: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## identityRemoveNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #identityRemoveNext:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyIdentityRemovingNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyIdentityRemovingNext:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## identityRemovePrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #identityRemovePrev:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyIdentityRemovingPrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyIdentityRemovingPrev:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## identityRemoveAll: removal 

		<primitive: #identityRemoveAll: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyIdentityRemovingAll: removal 

		<primitive: #copyIdentityRemovingAll: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## insert: infix at: insertionIndex 

		<primitive: #insert:at: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyInserting: infix at: insertionIndex 

		<primitive: #copyInserting:at: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## nextIndexOf: element startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #nextIndexOf:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## prevIndexOf: element startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #prevIndexOf:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## includes: element 

		<primitive: #includes: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## removeNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #removeNext:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingNext: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyRemovingNext:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## removePrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #removePrev:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingPrev: startIndex startingAt: removal ifAbsent: notFoundAction 

		<primitive: #copyRemovingPrev:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## removeAll: removal 

		<primitive: #removeAll: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingAll: removal 

		<primitive: #copyRemovingAll: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## atFirstPutOrAdd: newValue 

		<primitive: #atFirstPutOrAdd: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## atLastPutOrAdd: newValue 

		<primitive: #atLastPutOrAdd: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## setSize: newSize 

		<primitive: #setSize: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyWithSize: newSize 

		<primitive: #copyWithSize: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## firstIfNone: noSuchElementAction 

		<primitive: #firstIfNone: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## lastIfNone: noSuchElementAction 

		<primitive: #lastIfNone: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## nextIndexSuchThat: predicate startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #nextIndexSuchThat:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## prevIndexSuchThat: predicate startingAt: startIndex ifAbsent: noSuchElementAction 

		<primitive: #prevIndexSuchThat:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## contains: predicate 

		<primitive: #contains: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## addAll: suffix 

		<primitive: #addAll: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## , suffix

		<primitive: #, domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyFrom: startIndex to: endIndex 

		<primitive: #copyFrom:to: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyTo: endIndex 

		<primitive: #copyTo: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyFrom: startIndex 

		<primitive: #copyFrom: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## withAllButFirst

		<primitive: withAllButFirst domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## withAllButLast

		<primitive: withAllButLast domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## withFirst

		<primitive: withFirst domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## withLast

		<primitive: withLast domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## removeAt: index 

		<primitive: #removeAt: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingAt: index 

		<primitive: #copyRemovingAt: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyAndRemoveFrom: startIndex to: endIndex 

		<primitive: #copyAndRemoveFrom:to: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingFrom: startIndex to: endIndex 

		<primitive: #copyRemovingFrom:to: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## removeNextSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #removeNextSuchThat:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingNextSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #copyRemovingNextSuchThat:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## removePrevSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #removePrevSuchThat:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingPrevSuchThat: startIndex startingAt: predicate ifAbsent: notFoundAction 

		<primitive: #copyRemovingPrevSuchThat:startingAt:ifAbsent: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## removeAllSuchThat: predicate 

		<primitive: #removeAllSuchThat: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyRemovingAllSuchThat: predicate 

		<primitive: #copyRemovingAllSuchThat: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## insertAll: infix at: insertionIndex 

		<primitive: #insertAll:at: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyInsertingAll: infix at: insertionIndex 

		<primitive: #copyInsertingAll:at: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## moveFrom: startIndex to: stopIndex by: displacement 

		<primitive: #moveFrom:to:by: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyMovingFrom: startIndex to: stopIndex by: displacement 

		<primitive: #copyMovingFrom:to:by: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyReplacingFrom: startIndex to: stopIndex with: replacementSource startingAt: replacementSourceStartIndex 

		<primitive: #copyReplacingFrom:to:with:startingAt: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## reverse

		<primitive: reverse domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyReversed

		<primitive: copyReversed domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## reverseFrom: startIndex to: endIndex 

		<primitive: #reverseFrom:to: domain: IndexedHalfWordSlots>
	];

	protocol: #'system primitives' method:
	[## copyReversedFrom: startIndex to: endIndex 

		<primitive: #copyReversedFrom:to: domain: IndexedHalfWordSlots>
	]}}
