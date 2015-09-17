#include "MpdDbParser.h"
#include "MpdDbConnection.h"

#include "TSQLServer.h"
#include "TSQLResult.h"
#include "TSQLRow.h"
#include "TSQLStatement.h"
#include "TDatime.h"

#include "pugixml.hpp"

#include <iostream>
using namespace std;

struct search_walker : pugi::xml_tree_walker
{
    search_walker(string searchName){search_name = searchName;}

    string search_name;
    pugi::xml_node found_node;

    virtual bool for_each(pugi::xml_node& node)
    {
        if (node.name() == search_name)
        {
            found_node = node;
            return false;
        }

        return true; // continue traversal
    }
};

int MpdDbParser::ParseXml2Db(TString xmlName, TString schemaPath)
{
    pugi::xml_document docXML;
    pugi::xml_parse_result resultXML = docXML.load_file(xmlName);

    if (!resultXML)
    {
        cout<<"Error: reading XML file '"<<xmlName<<"' was failed"<<endl;
        return - -1;
    }

    // read schema
    pugi::xml_document docSchema;
    pugi::xml_parse_result resultSchema = docSchema.load_file(schemaPath);

    if (!resultSchema)
    {
        cout<<"Error: reading schema file '"<<schemaPath<<"' was failed"<<endl;
        return - 2;
    }

    // open connection to database
    MpdDbConnection* connUniDb = MpdDbConnection::Open(UNIFIED_DB);
    if (connUniDb == 0x00) return 0x00;
    TSQLServer* uni_db = connUniDb->GetSQLServer();

    pugi::xml_node cur_xml_node = docXML;
    string strTableName = "";
    // parse SCHEMA file
    for (pugi::xml_node_iterator it = docSchema.begin(); it != docSchema.end(); ++it)
    {
        pugi::xml_node cur_schema_node = *it;

        //cout<<"Current schema node: "<<cur_schema_node.name()<<endl;

        // parse table name if exists
        if (strcmp(cur_schema_node.attribute("table_name").value(), "") != 0)
        {
            strTableName = cur_schema_node.attribute("table_name").value();
            cout<<"Current database table: "<<strTableName<<endl;
        }

        if (strcmp(cur_schema_node.name(), "search") == 0)
        {
            string strSearchName = cur_schema_node.attribute("name").value();
            search_walker my_walker(strSearchName);
            cur_xml_node.traverse(my_walker);
            cur_xml_node = my_walker.found_node;

            cout<<"Current node after search: "<<cur_xml_node.name()<<endl;
        }
        else if (strcmp(cur_schema_node.name(), "move") == 0)
        {
            for (pugi::xml_attribute_iterator ait = cur_schema_node.attributes_begin(); ait != it->attributes_end(); ++ait)
            {
                if (strcmp(ait->name(), "down") == 0)
                {
                    int count = atoi(ait->value());
                    for (int i = 0; i < count; i++)
                        cur_xml_node = cur_xml_node.first_child();
                }// if attribute name is "down"
            }

            cout<<"Current node after move: "<<cur_xml_node.name()<<endl;
        }
        // PARSE CYCLE
        else if (strcmp(cur_schema_node.name(), "cycle") == 0)
        {
            string strChildName = cur_schema_node.attribute("child").value();

            vector<structParseSchema> vecElements;
            // write cycle structure to array vecElements
            int column_count = 0;
            for (pugi::xml_node cycle_child = cur_schema_node.first_child(); cycle_child; cycle_child = cycle_child.next_sibling())
            {
                string strAction = cycle_child.attribute("action").value();
                TString column_name = cycle_child.attribute("column_name").value();
                TString statement_type = cycle_child.attribute("type").value();

                if (strAction == "skip")
                {
                    structParseSchema par(true);

                    vecElements.push_back(par);
                }
                else if (strAction == "write")
                {
                    structParseSchema par(false, column_name, statement_type);

                    vecElements.push_back(par);
                    column_count++;
                }// if (strAction == "write")
                else if (strAction == "parse")
                {
                    structParseSchema par;
                    par.isSkip = false;

                    int start_index = atoi(cycle_child.attribute("start_index").value());
                    TString parse_type = cycle_child.attribute("parse_type").value();
                    structParseRow row(column_name, statement_type, true, start_index, parse_type);
                    par.vecRows.push_back(row);

                    vecElements.push_back(par);
                    column_count++;
                }// if (strAction == "parse")
                else if (strAction == "multi")
                {
                    structParseSchema par;
                    par.isSkip = false;

                    for (pugi::xml_node cycle_sub_child = cycle_child.first_child(); cycle_sub_child; cycle_sub_child = cycle_sub_child.next_sibling())
                    {
                        strAction = cycle_sub_child.attribute("action").value();
                        column_name = cycle_sub_child.attribute("column_name").value();
                        statement_type = cycle_sub_child.attribute("type").value();

                        if (strAction == "write")
                        {
                            structParseRow row(column_name, statement_type);
                            par.vecRows.push_back(row);
                        }
                        else if (strAction == "parse")
                        {
                            int start_index = atoi(cycle_sub_child.attribute("start_index").value());
                            TString parse_type = cycle_sub_child.attribute("parse_type").value();
                            structParseRow row(column_name, statement_type, true, start_index, parse_type);
                            par.vecRows.push_back(row);
                        }
                    }

                    vecElements.push_back(par);
                    column_count++;
                }// if (strAction == "multi")
            }// write cycle structure to array vecElements

            // prepare SQL insert for XML cycle
            TString sql = "insert into " + strTableName + "(";
            int count = 0;
            for (vector<structParseSchema>::iterator it = vecElements.begin(); it != vecElements.end(); ++it)
            {
                structParseSchema schema = *it;
                if (schema.isSkip)
                    continue;

                for (int j = 0; j < schema.vecRows.size(); j++)
                {
                    structParseRow row = schema.vecRows[j];
                    if (count == 0)
                        sql += row.strColumnName;
                    else
                        sql += ", " + row.strColumnName;

                    count++;
                }
            }
            sql += ") values(";
            for (int i = 1; i <= count; i++)
            {
                if (i == 1)
                    sql += TString::Format("$%d", i);
                else
                    sql += TString::Format(", $%d", i);
            }
            sql += ")";
            cout<<"SQL code: "<<sql<<endl;

            TSQLStatement* stmt = uni_db->Statement(sql);

            // run XML file cycle and write the fields to DB
            for (cur_xml_node = cur_xml_node.child(strChildName.c_str()); cur_xml_node; cur_xml_node = cur_xml_node.next_sibling(strChildName.c_str()))
            {
                count = 0;
                int i = 0;

                stmt->NextIteration();
                cout<<endl;

                // cycle for XML child elements
                for (pugi::xml_node cycle_child = cur_xml_node.first_child(); cycle_child; cycle_child = cycle_child.next_sibling(), i++)
                {
                    structParseSchema schema = vecElements[i];
                    if (schema.isSkip)
                        continue;

                    // cycle by schema rows because it can consist multiple writing
                    TString xml_child_value = cycle_child.first_child().value();
                    for (int j = 0; j < schema.vecRows.size(); j++)
                    {
                        structParseRow row = schema.vecRows[j];


                        if (!row.isParse)
                        {
                            if (row.strStatementType == "int")
                            {
                                stmt->SetInt(count, atoi(xml_child_value.Data()));
                                cout<<"SetInt: "<<xml_child_value.Data()<<endl;
                                count++;
                            }
                            else
                            {
                                if (row.strStatementType == "double")
                                {
                                    stmt->SetDouble(count, atof(xml_child_value.Data()));
                                    cout<<"SetDouble: "<<xml_child_value.Data()<<endl;
                                    count++;
                                }
                                else
                                {
                                    if (row.strStatementType == "string")
                                    {
                                        stmt->SetString(count,xml_child_value);
                                        cout<<"SetString: "<<xml_child_value<<endl;
                                        count++;
                                    }
                                    else
                                    {
                                        if (row.strStatementType == "datetime")
                                        {
                                            TDatime d(xml_child_value.Data());
                                            stmt->SetDatime(count, d);
                                            cout<<"SetDatime: "<<xml_child_value.Data()<<endl;
                                            count++;
                                        }
                                    }
                                }
                            }
                        }// !row.isParse
                        else
                        {
                            if (row.iStartIndex > 0)
                                xml_child_value = xml_child_value(row.iStartIndex, xml_child_value.Length()-row.iStartIndex);

                            if (row.strParseType != "")
                            {
                                if (row.strParseType == "int")
                                {
                                    int last_digit;
                                    for (last_digit = 0; last_digit < xml_child_value.Length(); last_digit++)
                                    {
                                        if (!isdigit(xml_child_value[last_digit]))
                                            break;
                                    }
                                    last_digit++;

                                    if (last_digit > 1)
                                        xml_child_value = xml_child_value(0, last_digit-1);
                                    else
                                        xml_child_value = "";
                                }
                            }

                            if (row.strStatementType == "int")
                            {
                                stmt->SetInt(count, atoi(xml_child_value.Data()));
                                cout<<"SetInt: "<<xml_child_value.Data()<<endl;
                                count++;
                            }
                            else
                            {
                                if (row.strStatementType == "double")
                                {
                                    stmt->SetDouble(count, atof(xml_child_value.Data()));
                                    cout<<"SetDouble: "<<xml_child_value.Data()<<endl;
                                    count++;
                                }
                                else
                                {
                                    if (row.strStatementType == "string")
                                    {
                                        stmt->SetString(count,xml_child_value);
                                        cout<<"SetString: "<<xml_child_value<<endl;
                                        count++;
                                    }
                                    else
                                    {
                                        if (row.strStatementType == "datetime")
                                        {
                                            TDatime d(xml_child_value.Data());
                                            stmt->SetDatime(count, d);
                                            cout<<"SetDatime: "<<xml_child_value.Data()<<endl;
                                            count++;
                                        }
                                    }
                                }
                            }
                        }// row.isParse

                    }// cycle by schema rows because it can consist multiple writing
                }// cycle for XML child elements
            }// run XML file cycle and write the fields to DB

            stmt->Process();
            delete stmt;
        }// CYCLE PROCESSING
    }// for docSchema level 0

    return 0;
}

// -------------------------------------------------------------------
ClassImp(MpdDbParser);
