# Mark Sweep

### Commands

```sh
mkdir cmake-build-debug && cd cmake-build-debug
# now we in build dir
cmake ..
make # generate binary
# or
make test # run test
```

### Introduction

How mark-sweep work?

It split to two part, `mark` & `sweep`

`Collect` pseudo code

```
Collect():
	MarkFrom(Root)
	Sweep()
```

Pseudo Code

- Mark
	```
	MarkFrom(Root):
		if Root.isMarked != true:
			Root.isMarked = true
		foreach reachable Object in Root:
			MarkFrom(object)
	```
- Sweep
	```
	Sweep():
		foreach object in Heap:
			if object.isMarked:
				object.isMarked = false
			else:
				release(object)
	```
