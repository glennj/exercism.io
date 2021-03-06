CREATE OR REPLACE PACKAGE hello_world# IS
   FUNCTION hello(
      i_name                                        varchar2 := ''
   ) 
      RETURN varchar2;

END hello_world#;
/

CREATE OR REPLACE PACKAGE BODY hello_world# IS
   FUNCTION hello(
      i_name                                        varchar2 := ''
   ) 
      RETURN varchar2
   AS
   BEGIN
      RETURN 'Hello, ' || NVL(i_name, 'World') || '!';
   END hello;
END hello_world#;
/
