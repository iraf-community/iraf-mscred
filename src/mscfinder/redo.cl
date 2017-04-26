procedure redo (output)

string	output			{prompt="Output root name"}
bool	overwrite = yes		{prompt="Overwrite the previous files?"}

begin
	string	loutput

	loutput = output

	if (overwrite) {
	    delete (loutput // ".out", ver-, >& "dev$null")
	    delete (loutput // ".ast", ver-, >& "dev$null")
	    delete (loutput // ".coo", ver-, >& "dev$null")
	}

	astrom (osfn(loutput//".in"), > loutput // ".out")

	rename ("astrom.lis", loutput // ".ast")
end
