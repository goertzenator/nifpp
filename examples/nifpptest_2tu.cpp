/*
 * This is a second translation unit for the nifpptest nif module to test
 * linkage behavior of resource data
 *
 */

#include "nifpp.h"


nifpp::resource_ptr<int> get_resource_int(int val)
{
    auto ptr = nifpp::construct_resource<int>(val);
    return ptr;
}
