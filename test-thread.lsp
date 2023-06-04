(load "libwdb.o")
(load "collections.lsp")

(with-wdb-database "thread-test.g"
                   (threading-tool "tap.c" 
                                   #(0 0 0) 
                                   #(0 1 0)
                                   #(0 0 3)
                                   0.3
                                   0.01
                                   'tap))                   


