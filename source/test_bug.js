function getPropNames(obj)
{ 
    var curObj = obj;
    var curIdx = 0;

    // Move to the next available property
    function nextProp()
    {
        while (true)
        {
            // Get a pointer to the hash table
            var tblPtr = get_obj_tbl(curObj);

            // Get the size of the hash table
            var tblSize = iir.icast(
                IRType.pint,
                get_obj_tblsize(curObj)
            );

            // Until the key is found, or a free slot is encountered
            while (true)
            {
                if (unboxInt(curIdx) < tblSize)
                    break;
            }

            //break;
        }
    }
}
