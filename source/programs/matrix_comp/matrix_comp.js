function Matrix(numRows, numCols)
{
    this.rows = [];

    for (var i = 0; i < numRows; ++i)
    {
        this.rows[i] = [];

        for (var j = 0; j < numCols; ++j)
        {
            this.rows[i][j] = 0;
        }
    }
}

Matrix.prototype = {};

Matrix.prototype.getNumRows = function ()
{
    return this.rows.length;
}

Matrix.prototype.getNumCols = function ()
{
    return this.rows[0].length;
}

Matrix.prototype.setElem = function (row, col, val)
{
    this.rows[row][col] = val;
}

Matrix.prototype.getElem = function (row, col)
{
    return this.rows[row][col];
}

Matrix.prototype.set = function (array)
{
    if (array.length != this.getNumRows() * this.getNumCols())
        throw new RangeError('invalid array in set');

    var idx = 0;

    for (var i = 0; i < this.getNumRows(); ++i)
    {
        for (var j = 0; j < this.getNumCols(); ++j)
        {
            this.rows[i][j] = array[idx++];
        }
    }
}

Matrix.prototype.sum = function ()
{
    var sum = 0;

    for (var i = 0; i < this.getNumRows(); ++i)
    {
        for (var j = 0; j < this.getNumCols(); ++j)
        {
            sum += this.rows[i][j];
        }
    }

    return sum;
}

Matrix.prototype.toString = function ()
{
    var outStr = '[';

    for (var i = 0; i < this.getNumRows(); ++i)
    {
        outStr += this.rows[i];

        if (i != this.getNumRows() - 1)
            outStr += ';'
    }

    outStr += ']';    

    return outStr;
}

Matrix.elemOp = function (inMatrix, opFunc)
{
    var outMatrix = new Matrix(inMatrix.getNumRows(), inMatrix.getNumCols());

    for (var i = 0; i < inMatrix.getNumRows(); ++i)
    {
        for (var j = 0; j < inMatrix.getNumCols(); ++j)
        {
            outMatrix.setElem(i, j, opFunc(inMatrix.getElem(i, j)));
        }
    }

    return outMatrix;
}

Matrix.scalarMult = function (inMatrix, scalar)
{
    return Matrix.elemOp(
        inMatrix,
        function (v) { return v * scalar }
    );
}

Matrix.matrixMult = function (mat1, mat2)
{
    if (mat1.getNumCols() != mat2.getNumRows())
        throw TypeError('matrix dimensions do not match');

    var outMatrix = new Matrix(mat1.getNumRows(), mat2.getNumCols());
    
    for (var i = 0; i < mat1.getNumRows(); ++i)
    {
        for (var j = 0; j < mat2.getNumCols(); ++j)
        {
            var sum = 0;

            for (var k = 0; k < mat1.getNumCols(); ++k)
            {
                sum += mat1.getElem(i, k) * mat2.getElem(k, j);
            }

            outMatrix.setElem(i, j, sum);
        }
    }

    return outMatrix;
}

function test()
{
    var m1 = new Matrix(2, 4);
    var m2 = new Matrix(4, 3);

    m1.set(
        [
            2, 0,-1, 1,
            1, 2, 0, 1
        ]
    );

    m2.set(
        [
            1, 5,-7,
            1, 1, 0,
            0,-1, 1,
            2, 0, 0
        ]
    );

    var res = Matrix.matrixMult(Matrix.scalarMult(m1, 2), m2);

    var resStr = String(res);

    var validStr = "[8,22,-30;10,14,-14]";

    if (resStr != validStr)
        return 1;

    var resSum = res.sum();

    if (resSum != 10)
        return 2;    

    return resSum;
}

