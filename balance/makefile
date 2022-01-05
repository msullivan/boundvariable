all : balance balance.um

balance : balance.mlb *.sml
	mlton -expert true -profile time -const 'MLton.safe false' balance.mlb

balance.um: balance-main.uml balance.uh
	../um/humlock/humlock -sequence-unit balance-main.uml
