$session = [Guid]::NewGuid()

$databaseUser = 'postgres'

function With-Database([ScriptBlock] $body) {
    pg_isready
    If (!$?) {
        Throw 'PostgreSQL is not ready.'
    }

    createdb -U "$databaseUser" "dinote-test-$session"
    if (!$?) {
        Throw 'Cannot create database.'
    }
    Try {
        $body.Invoke()
    } Finally {
        dropdb -U "$databaseUser" "dinote-test-$session"
        if (!$?) {
            Throw 'Cannot drop database.'
        }
    }
}

With-Database { }
