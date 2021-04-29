# Copyright (C) 2020 Brodie Gaslam
#
# This file is part of "vetr - Trust, but Verify"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Trust, but Verify
#'
#' Declarative template-based framework for verifying that objects meet
#' structural requirements, and auto-composing error messages when they do not.
#'
#' @name vetr-package
#' @docType package
#' @importFrom methods new

NULL

# importFrom methods needed for tests of reference classes because we don't want
# to create the classes in the tests due to the topenv issues.
