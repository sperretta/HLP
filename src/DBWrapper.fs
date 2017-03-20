namespace DBWrapper

open databaseStructure

module DBWrapper =
    let database : databaseStructure.database = ref databaseStructure.INilTable
    let execute fn = fn database