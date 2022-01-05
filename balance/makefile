default : balance

balance : balance.cm *.sml
	mlton -expert true -profile time -const 'MLton.safe false' balance.cm

balance.um: balance-main.uml balance.uh
	../um/humlock/humlock -sequence-unit balance-main.uml
