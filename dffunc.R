require(reshape2)

# Order of functions
# - Generic (GERRIT) table
# - CHANGE
# - FILE
# - GITF (FILE of GIT info)
# - PATCHSET
# - REVIEW
# - CASTED


GERRIT.merge.CHANGE <- function(GERRIT, CHANGE){
    # Merge any of Gerrit table (Patchsets/Reviews) with Changes table to get ID
    #
    # Args:
    #   CHANGE: CHANGE table with 'project' column
    #
    # Returns:
    #   CASTED: GERRIT with a 'project' column
    merge(x=CHANGE[,c("number","project")], y=GERRIT,
          by.x="number", by.y="number", all.x=FALSE, all.y=FALSE)
}

GERRIT.merge.PATCHSET <- function(GERRIT, PATCHSET){
    # Merge GERRIT and PACHSET
    #
    # Args:
    #   GERRIT:      GERRIT table
    #   PATCHSET:    PATCHSET table
    #
    # Returns:
    #   GERRIT: with the latest PATCHSET data (Not all of working patchsets)
    GERRIT$id <- with(GERRIT, paste(number, num_patches, sep="/"))
    X <- merge(GERRIT, PATCHSET[,c("id", "display.name")], by="id", all.x=TRUE, all.y=FALSE)
}

CHANGE.read <- function(path, projects=c(), proj.length=150, level=0.1){
    # Read CHANGE table in right formats
    #
    # Args:
    #   path:        path to CSV file
    #
    #   (Following two parameters are used to calculate the decay factor)
    #   proj.length: Specified day at which the weight reaches 'level'
    #                A good choice will be an approximated project length
    #   level:       Default is 0.1, it must be within 0~1
    #
    # Returns:
    #   CHANGE: Format changed CHANGE table
    colClasses <- c("character", rep("NULL", 2), rep("factor", 3), "integer", rep("character", 2),
                    rep("factor", 3))
    CHANGE <- read.csv(file=path, colClasses=colClasses)
    CHANGE <- subset(CHANGE, !project %in% projects)
    CHANGE[CHANGE$date_closed=="",]$date_closed <- CHANGE[CHANGE$date_closed=="",]$date_created
    CHANGE$date_created <- as.POSIXlt(CHANGE$date_created)
    CHANGE$date_closed <- as.POSIXlt(CHANGE$date_closed)
    CHANGE[CHANGE$date_closed==CHANGE$date_created,]$date_closed <- CHANGE[CHANGE$date_closed==CHANGE$date_created,]$date_closed + (3600 * 24)
    date.last <- max(CHANGE$date_closed)
    psi <- log(level) / proj.length   # Decay factor psi is chosen as the weight becomes 'level' at
                                      # 'proj.length' days ago
    CHANGE$age <- round(as.numeric(date.last - CHANGE$date_closed, units="days"), 0)
    CHANGE$weight <- round(exp(psi * CHANGE$age), 2)
    CHANGE
}

CHANGE.merge.FILE <- function(CHANGE, PATCHSET.merge.FILE){
    # CHANGE table with FILE summary info
    #
    # Args:
    #   CHANGE:               CHANGE table
    #   PATCHSET.merge.FILE:  PATCHSET table
    #
    # Returns:
    #   CHANGE: with (churn, nof, entropy)
    CHANGE$id <- with(CHANGE, paste(number, num_patches, sep="/"))
    merge(CHANGE, PATCHSET.merge.FILE[,c("id", "churn", "nof")], by="id", all.x=TRUE)
}


CHANGE.review.effectiveness <- function(PATCHSET.merge.FILE){
    # Calculate the normalized variation over the churn numbers of patches in a commit
    #
    # Args:
    #   PATCHSET.merge.FILE
    #
    # Returns:
    #   CHANGE: (number, patches.sd) where patches.sd has no unit
    agg0 <- aggregate(PATCHSET.merge.FILE[,c("churn")], by=list(project=PATCHSET.merge.FILE$number),
                      FUN=function(y){
                          M <- mean(y)
                          sd(y) / M
                      })
    names(agg0) <- c("number", "patches.sd")
    agg0[is.na(agg0$patches.sd), "patches.sd"] <- 0
    agg0
}

CHANGE.summary.REVIEW <- function(CHANGE, PATCHSET, REVIEW){
    # CHANGE table with Review status
    #
    # Args:
    #   CHANGE:      CHANGE table
    #   PATCHSET:    PATCHSET table
    #   REVIEW:      Info-extracted REVIEW table
    #
    # Returns:
    #   CHANGE: (number/length of comments/inlines/reviewers, self.approval/verification)
    REVIEW.local <- REVIEW
    CHANGE.local <- CHANGE
    PATCHSET.local <- PATCHSET
    REVIEW.local$id <- with(REVIEW.local, paste(number, revision_number, sep="/"))
    CHANGE.local <- GERRIT.merge.PATCHSET(CHANGE.local, PATCHSET.local) # To get the latest patch author (display.name)
    # Approver
    approving.REVIEW <- subset(REVIEW.local, rscore==2)
    reviewed.CHANGE <- merge(approving.REVIEW[,c("reviewer", "id")], CHANGE.local, by="id", all.x=FALSE, all.y=TRUE)
    names(reviewed.CHANGE)[which(names(reviewed.CHANGE)=="reviewer")] <- "approver"

    # Verifier
    verifying.REVIEW <- subset(REVIEW.local, vscore==1)
    reviewed.CHANGE <- merge(verifying.REVIEW[,c("reviewer", "id")], reviewed.CHANGE, by="id", all.x=FALSE, all.y=TRUE)
    names(reviewed.CHANGE)[which(names(reviewed.CHANGE)=="reviewer")] <- "verifier"

    # Remove duplicated rows (some commits e.g. 937665, 939514 have double approvals)
    reviewed.CHANGE <- reviewed.CHANGE[!duplicated(reviewed.CHANGE$id), ]
    # Still exists
    # Common columns for length parameters
    names(REVIEW.local)[which(names(REVIEW.local)=="revision_number")] <- "num_patches"
    REVIEW.edit <- GERRIT.merge.PATCHSET(REVIEW.local, PATCHSET.local)

    REVIEW.edit$is.self <- REVIEW.edit$reviewer == REVIEW.edit$display.name
    melted <- melt(REVIEW.edit, measure.vars=c("inlines", "comments"), id.vars=c("number", "is.self"))
    melted <- subset(melted, !is.na(melted$is.self)) # Some patches are not available e.g. 916075 due to Draft

    # Length of comments (self, others)
    casted1 <- dcast(melted, formula=number~variable+is.self, fun.aggregate=sum)
    names(casted1) <- c("number", "L.inlines.others", "L.inlines.self", "L.comments.others", "L.comments.self")

    # Number of discussions
    casted2 <- dcast(melted, formula=number~variable+is.self, fun.aggregate=length)
    names(casted2) <- c("number", "N.inlines.others", "N.inlines.self", "N.comments.others", "N.comments.self")

    # Number of review participants
    melted3 <- melt(REVIEW.edit, measure.vars=c("reviewer"), id.vars=c("number"))
    casted3 <- dcast(melted3, formula=number~variable,
                     fun.aggregate=function(x){length(levels(as.factor(x)))})
    names(casted3) <- c("number", "N.reviewers")
    casted <- merge(casted1, casted2, by="number")
    casted <- merge(casted, casted3, by="number")

    # Check for self-score
    reviewed.CHANGE$approver <- as.character(reviewed.CHANGE$approver)
    if (sum(is.na(reviewed.CHANGE$approver)) > 0) {
        reviewed.CHANGE[is.na(reviewed.CHANGE$approver),]$approver <- "nobody.approved"
    }
    reviewed.CHANGE$verifier <- as.character(reviewed.CHANGE$verifier)
    if (sum(is.na(reviewed.CHANGE$verifier)) > 0) {
        reviewed.CHANGE[is.na(reviewed.CHANGE$verifier),]$verifier <- "nobody.verified"
    }
    reviewed.CHANGE$self.approval <- (reviewed.CHANGE$approver == reviewed.CHANGE$display.name)
    reviewed.CHANGE$self.verification <- reviewed.CHANGE$verifier == reviewed.CHANGE$display.name
    reviewed.CHANGE <- merge(reviewed.CHANGE, casted, by="number")
    subset(reviewed.CHANGE, select=-c(id))
}

FILE.read <- function(FILE.raw, file.type=".*"){
    # Read FILE table
    #
    # Args:
    #   FILE.raw:      RAW CSV table object
    #   file.type: Regexp to filter out files
    #
    # Returns:
    #   FILE: rows with filtered type, with 'churn' column
    FILE <- FILE.raw
    FILE <- FILE[grepl(pattern=file.type, x=FILE$file),]
    FILE$churn <- FILE$lines_add + FILE$lines_del
    FILE[FILE$churn==0,]$churn <- 1
    FILE <- subset(FILE, select=-c(status, lines_add, lines_del))
    FILE
}

GIT.format.GERRIT <- function(GIT){
    names(GIT)[which(names(GIT)=="hexsha")] <- "number"
    names(GIT)[which(names(GIT)=="lines")] <- "churn"
    if ("hexsha.more" %in% names(GIT)){
        names(GIT)[which(names(GIT)=="hexsha.more")] <- "number.more"
    }
    GIT
}

GITF.read <- function(path, file.type=".*"){
    # Read FILE table
    #
    # Args:
    #   path:      Path to the CSV file
    #   file.type: Regexp to filter out files
    #
    # Returns:
    #   FILE: rows with filtered type, with 'churn' column
    FILE <- read.csv(path)
    FILE <- FILE[grepl(pattern=file.type, x=FILE$file),]
    FILE[FILE$lines==0,]$lines <- 1
    subset(FILE, select=-c(insertions, deletions))
}

PATCHSET.clean <- function(PATCHSET.raw, committers){
    # Replace auto-filled Gerrit committer's name with a guessed person's name
    #
    # Args:
    #   PATCHSET:    PATCHSET table
    #   committers:  list of committters to be replaced
    #   reviewers: list of reviewers to remove
    #
    # Returns:
    #   PATCHSET: Cleaned PATCHSET
    PATCHSET.raw$committer <- as.character(PATCHSET.raw$committer)
    #bool <- PATCHSET.raw$committer %in% committers
    #PATCHSET.raw[bool,]$committer <- as.character(PATCHSET.raw[bool,]$author)
    PATCHSET.raw
}

FILE.summary <- function(FILE, clustering=TRUE){
    # Summarize FILE information and returns CHANGE record
    #
    # Args:
    #   FILE: Either a pure FILE or 'FILE.dir.clustering'
    #
    # Returns:
    #   PATCHSET with FILE summary info
    #   "number" "revision_number" "churn" "nof" "project"
    #   "project.less" "number.less" "project.more" "number.more"
    FI <- FILE

    local.MAP.proj.num <- subset(FI, select=c(number, project))
    if("project.more" %in% names(FILE)){
        FI <- CHANGE.use.more(FI, number=TRUE, project=TRUE)
        # Update the MAP table using a new 'number'
        local.MAP.proj.num <- subset(FI, select=c(number, project, project.less))
    }
    local.MAP.proj.num <- local.MAP.proj.num[!duplicated(local.MAP.proj.num$number),]
    row.names(local.MAP.proj.num) <- 1:nrow(local.MAP.proj.num)

    agg1 <- aggregate(FI[, c("churn")], by=list(number=FI$number, revision_number=FI$revision_number), sum)
    names(agg1)[3] <- "churn"
    agg1$id <- paste(agg1$number, agg1$revision_number, sep="/")

    agg2 <- aggregate(FI[, c("churn")], by=list(number=FI$number, revision_number=FI$revision_number), length)
    names(agg2)[3] <- "nof"
    agg2$id <- paste(agg2$number, agg2$revision_number, sep="/")

    agg <- merge(agg1, agg2[,c("id", "nof")], by="id")
    agg <- merge(agg, local.MAP.proj.num, by="number", all=TRUE)
    if(clustering){
        agg$number.less <- sub(x=agg$number, pat="([0-9]*)/.*", rep="\\1")
        agg <- CHANGE.use.less(agg, number=TRUE, project=TRUE)
    }
    subset(agg, select=-c(id))
}

GITF.summary <- function(GITF){
    TMP <- GITF
    agg <- aggregate(TMP[, c("churn")], by=list(number=TMP$number), sum)
    names(agg)[2] <- "churn"
    local.MAP <- GITF[!duplicated(GITF$number), c("number", "project")]
    merge(agg, local.MAP, by="number", all.x=TRUE, all.y=FALSE)
}

PATCHSET.merge.FILE <- function(PATCHSET, summarized.FILE){
    # Merge FILE summary to PATCHSET
    #
    # Args:
    #   PATCHSET: PATCHSET table
    #   summarized.FILE: created by FILE.summary
    #
    # Returns:
    #   PATCHSET with FILE summary info
    FI <- summarized.FILE
    FI <- GERRIT.add.ID(FI)
    PATCHSET <- GERRIT.add.ID(PATCHSET)
    P <- merge(PATCHSET, subset(FI, select=-c(number, revision_number, project)),  by="id", all.x=FALSE, all.y=TRUE)
    subset(P, select=-c(id))
}

PATCHSET.merge.REVIEW <- function(PATCHSET, REVIEW.raw, committers){
    # Merge PATCHSET with REVIEW to get display.name
    #
    # Args:
    #   PATCHSET:   read by PATCHSET.read
    #   REVIEW.raw: read by read.csv
    #
    # Returns:
    #   PATCHSET: with display.name

    MSG_UPLOAD <- "^Uploaded patch set [0-9]*.$"
    MSG_UPDATE_MSG = "^Patch Set [0-9]*: Commit message was updated$"
    MSGS_NOTIFICATION <- c(MSG_UPLOAD, MSG_UPDATE_MSG)
    MSG_REB <- "^Patch Set [0-9]*: Patch Set [0-9]* was rebased$"

    # Get R.name table to get the name from REVIEW.raw
    # MSG_NOTIFICATION tells about the name
    R <- REVIEW.raw
    R$id <- paste(R$number, R$revision_number, sep="/")
    R$message <- as.character(R$message)
    bool <- rep(FALSE, nrow(R))
    for (template in MSGS_NOTIFICATION){
        bool <- bool + grepl(pattern = template, x = R$message)
    }
    bool <- as.logical(bool)
    R.name <- R[bool, c("id", "reviewer")]

    # Rebase message tells about display.name for a new patch
    bool <- grepl(pattern = MSG_REB, x = R$message)
    R[bool,]$revision_number <- R[bool,]$revision_number + 1
    R$id <- paste(R$number, R$revision_number, sep="/")
    R.name <- rbind(R.name, R[bool, c("id", "reviewer")])

    # Merge REVIEW.raw to get 'display.name'
    P <- PATCHSET
    P$id <- paste(P$number, P$revision_number, sep="/")
    M <- merge(P, R.name, by="id", all.x=TRUE, all.y=FALSE) # All PATCHSET record is usable
    M$reviewer <- as.character(M$reviewer)

#     # TODO: If committer is "xsjpsoft06@sonymobile.com" or other auto, replace it by the author's email
#     #       And make its reviewer's field NA
#     bool <- M$reviewer %in% committers
#     M[bool,]$reviewer <- NA

    # Still some are NA, then get it from 'committer' (email address)
    tmp <- sub(M[which(is.na(M$reviewer)),]$"committer", pat="([a-z0-9]*)\\.([a-z0-9].*)@.*", repl="\\1 \\2")
    tmp <- sub(tmp, pat="x\\.", repl="")
    tmp <- sub(tmp, pat="xa\\.", repl="")

    .simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
    }
    tmp <- unname(sapply(tmp, .simpleCap))
    M[which(is.na(M$reviewer)),]$"reviewer" <- tmp

    # Finally change the colname to 'display.name'
    names(M)[length(M)] <- "display.name"
    subset(M, select=-c(id))
}

PATCHSET.read <- function(PATCHSET.raw, REVIEW.raw, committers){
    # Read PATCHSET table in right formats (Wrapper of PATCHSET.clean)
    #
    # Args:
    #   CHANGE: CHANGE table
    #
    # Returns:
    #   CHANGE: Format changed CHANGE table
    PATCHSET <- PATCHSET.clean(PATCHSET.raw, committers)
    PATCHSET <- PATCHSET.merge.REVIEW(PATCHSET, REVIEW.raw, committers)
    PATCHSET
}

REVIEW.clean <- function(REVIEW, reviewers, projects=c(), notification=TRUE){
    # Remove auto, notification; Classify messages and remove NOTIFICATION
    #
    # Args:
    #   REVIEW:    REVIEW table with `project` column (Use GERRIT.merge.CHANGE)
    #   projects:  list of projects to remove
    #   reviewers: list of reviewers to remove
    #
    # Returns:
    #   REVIEW: Cleaned REVIEW with a 'class' column
    MSG_REB <- "^Patch Set [0-9]*: Patch Set [0-9]* was rebased$"
    MSG_UPLOAD <- "^Uploaded patch set [0-9]*.$"
    MSG_ABANDONED = "^Abandoned$"
    MSG_MERGED <- "^Change has been successfully merged into the git repository.$"
    MSG_UPDATE_MSG = "^Patch Set [0-9]*: Commit message was updated$"
    MSG_CHERRYPICK = "^Patch Set [0-9]*: Cherry Picked"
    MSGS_NOTIFICATION <- c(MSG_REB, MSG_UPLOAD, MSG_ABANDONED, MSG_MERGED, MSG_UPDATE_MSG, MSG_CHERRYPICK)

    R <- REVIEW
    R <- subset(R, ! project %in% projects)
    R <- subset(R, ! reviewer %in% reviewers)
    R$message <- as.character(R$message)
    if(notification){
        for (template in MSGS_NOTIFICATION){
            indexes <- grep(pattern = template, x = R$message)
            if(length(indexes) > 0){
                R <- R[-indexes,]
            }
        }
    }
    R
}

REVIEW.extract.info <- function(REVIEW){
    # Interprete the review message and translate it into integer values
    #
    # Args:
    #   REVIEW: REVIEW table
    #
    # Returns:
    #   REVIEW: with (vscore, rscore, inlines, comments)

    # Temporary constant
    MSG_HEADER <- "^Patch Set [0-9]*:( |\n\n)"

    MSG_VSCORE <- "Verified([-+]1)"
    MSG_VSCORE_1 <- "(Verified[+]1|Verified\n\n)"
    MSG_VSCORE_m1 <- "Verified[-]1"

    MSG_REV_m1<- "(Code[-]Review[-]1|I would prefer that you didn't submit this\n\n)"
    MSG_REV_m2<- "Code[-]Review[-]2"
    MSG_REV_1 <- "(Code[-]Review[+]1|Looks good to me, but someone else must approve\n\n)"
    MSG_REV_2 <- "(Code[-]Review[+]2|Looks good to me, approved\n\n)"
    MSG_PUSHED = "^Change has been successfully pushed.$"

    MSG_INLINES <- ".*\\(([0-9]*)( inline)? comment[s]?\\)"

    R <- REVIEW

    # Remove header message like "Patch Set 1: "
    R$message <- sub(pattern=MSG_HEADER, repl="", x=R$message)

    # Verified scores
    R$vscore <- 0
    indexes <- which(grepl(pattern = MSG_VSCORE_1, x=R$message))
    R$message[indexes] <- sub(pattern=MSG_VSCORE_1, repl="", x=R$message[indexes])

    R$vscore[indexes] <- 1
    indexes <- which(grepl(pattern = MSG_VSCORE_m1, x=R$message))
    R$vscore[indexes] <- -1
    R$message[indexes] <- sub(pattern=MSG_VSCORE_m1, repl="", x=R$message[indexes])

    # Review scores
    R$rscore <- 0
    indexes <- which(grepl(pattern = MSG_REV_m1, x=R$message))
    R$rscore[indexes] <- -1
    R$message[indexes] <- sub(pattern=MSG_REV_m1, repl="", x=R$message[indexes])
    indexes <- which(grepl(pattern = MSG_REV_m2, x=R$message))
    R$rscore[indexes] <- -2
    R$message[indexes] <- sub(pattern=MSG_REV_m2, repl="", x=R$message[indexes])
    indexes <- which(grepl(pattern = MSG_REV_1, x=R$message))
    R$rscore[indexes] <- 1
    R$message[indexes] <- sub(pattern=MSG_REV_1, repl="", x=R$message[indexes])
    indexes <- which(grepl(pattern = MSG_REV_2, x=R$message))
    R$rscore[indexes] <- 2
    R$message[indexes] <- sub(pattern=MSG_REV_2, repl="", x=R$message[indexes])
    indexes <- which(grepl(pattern = MSG_PUSHED, x=R$message))
    R$rscore[indexes] <- 2
    R$message[indexes] <- sub(pattern=MSG_PUSHED, repl="", x=R$message[indexes])

    # Inline comments
    R$inlines <- "0"
    indexes <- which(grepl(pattern = MSG_INLINES, x=R$message))
    R$inlines[indexes] <- sub(pattern = paste(MSG_INLINES, ".*", sep=""), replacement = "\\1",
                                   x = R$message[indexes])
    R$message[indexes] <- sub(pattern=MSG_INLINES, repl="", x=R$message[indexes])
    R$inlines <- as.integer(R$inlines)

    # Count the length of messages after removing formatted message chunk
    R$comments <- sapply(X=R$message, FUN=function(x){length(unlist(strsplit(x, split=" ")))})
    R <- subset(R, select=-c(message))
    R
}

REVIEW.read <- function(REVIEW.raw, MAP.num.proj, reviewers=c(), projects=c(), notification=TRUE){
    # Read review data with MAP.num.proj columns, message is not translated yet
    #
    # Args:
    #   path:         path to the file
    #   MAP.num.proj:       MAP.num.proj table
    #   projects:     to be excluded
    #   reviewers:    to be excluded
    #   notification: notification type message is kept if TRUE, removed otherwise
    #
    # Returns:
    #   REVIEW.raw:  with (vscore, rscore, inlines, comments)
    REVIEW.proc1 <- GERRIT.merge.CHANGE(REVIEW.raw, MAP.num.proj)
    REVIEW.proc2 <- REVIEW.clean(REVIEW = REVIEW.proc1, reviewers, projects, notification)
    REVIEW <- REVIEW.extract.info(REVIEW.proc2)
    REVIEW
}

CASTED.changes.count <- function(CHANGE, weight=FALSE, simple=FALSE){
    # Count number of patchset, commit appearances of CHANGE
    #
    # Args:
    #   CHANGE: CHANGE table
    #
    # Returns:
    #   CASTED: (project, num_commits, num_patches, many_patches ratio, cherry-pick, revert)
    FUN <- function(X, fun, name, use.weight=weight){
        if(use.weight){X <- X * C$weight}
        agg <- aggregate(x=X, by=list(C$project), FUN=fun)
        names(agg) <- c("project", name)
        agg
    }

    if(simple){
        C <- CHANGE[,c("project", "number")]
        C$dummy <- 1
        OUT <- FUN(C$dummy, sum, "num_commits")
        return(OUT)
    }
    C <- CHANGE[,c("project", "num_patches", "created_by", "weight")]
    C$revert <- (C$created_by == "revert")
    C$cherrypick <- (C$created_by == "cherry-pick")
    C$many.patches <- (C$num_patches >= 4)
    C$dummy <- 1

    OUT <- FUN(C$revert, sum, "revert", use.weight=FALSE)
    OUT <- merge(by="project", x=OUT, y=FUN(C$cherrypick, sum, "cherrypick", use.weight=FALSE))
    OUT <- merge(by="project", x=OUT, y=FUN(C$dummy, sum, "num_commits"))
    OUT <- merge(by="project", x=OUT, y=FUN(C$num_patches, sum, "num_patches"))
    OUT <- merge(by="project", x=OUT, y=FUN(C$many.patches, sum, "many_patches", use.weight=FALSE))
    OUT
}

CASTED.committers.count <- function(PATCHSET){
    # Count number of committers of PATCHSET
    #
    # Args:
    #   PATCHSET: PATCHSET table
    #
    # Returns: CASTED
    #   id:      (project, display.name)
    #   metrics: (Freq, commits, ratio, major, eff.major)

    C <- FLTR.latest(PATCHSET)
    agg.name.proj <- aggregate(x=C$number, by=list(display.name=C$display.name, project=C$project), FUN=length)
    names(agg.name.proj) <- c("committer", "project", "Freq")
    agg.proj <- aggregate(x=C$number, by=list(project=C$project), FUN=length)
    names(agg.proj) <- c("project", "commits")
    agg <- merge(agg.name.proj, agg.proj, by="project", all=TRUE)
    agg$ratio <- agg$Freq / agg$commits
    agg$major <- agg$ratio >= 0.05
    agg$eff.major <- (agg$major & agg$Freq >1) # Effective Major, major must have more than 2 commits
    agg
}

CASTED.GIT <- function(GITF, GITC){
    # Count the number of lines of code churn in GIT instead of GERRIT
    #
    # Args:
    #   GITF: GIT FILE table read by GITF.read
    #   GITC: GIT CHANGE table
    #
    # Returns:
    #   CASTED: (project, lines)

    # agg0 for the code churn by change
    agg0 <- aggregate(x=GITF[, "lines"], by=list(GITF$hexsha), FUN=sum)
    names(agg0) <- c("hexsha", "lines")
    TMP <- merge(GITC[, c("project", "hexsha")], agg0, by="hexsha", all.x=TRUE, all.y=FALSE)
    TMP[is.na(TMP$lines), ]$lines <- 0
    agg1 <- aggregate(x=TMP[, "lines"], by=list(TMP$project), FUN=sum)
    names(agg1) <- c("project", "git.lines")
    # agg2 for the number of commits
    agg2 <- aggregate(x=TMP[, "project"], by=list(TMP$project), FUN=length)
    names(agg2) <- c("project", "git.commits")
    merge(agg1, agg2, by="project", all=TRUE)
}

CASTED.entropy <- function(FILE){
    # Calculate the Shannon entropy and total edited files (length, number)
    #
    # Args:
    #   FILE:   FILE table of latest patchset only
    #
    # Returns:
    #   CASTED: (project, entropy, nof, churn)
    #           where the figures are based on commits not snapshot
    FI <- FILE

    FI$file <- paste(FI$project, FI$file, sep=":")
    # preagg is summation of churn over files
    preagg <- aggregate(FI[, c("churn")], by=list(FI$file), sum)
    names(preagg) <- c("file", "churn")
    preagg <- preagg[!is.na(preagg$churn),]
    preagg$project <- sub(pattern=":.*", repl="", x=preagg$file)

    # agg1 to get entropy
    agg1 <- aggregate(preagg[, c("churn")], by=list(preagg$project), entropy)
    names(agg1) <- c("project", "entropy")
    # agg2 to get number of files
    agg2 <- aggregate(preagg[, c("churn")], by=list(preagg$project), length)
    names(agg2) <- c("project", "nof")
    # agg3 to get loc of all changes
    agg3 <- aggregate(preagg[, c("churn")], by=list(preagg$project), sum)
    names(agg3) <- c("project", "churn")
    agg <- merge(agg1, agg2, by="project")
    agg <- merge(agg, agg3, by="project")
    agg
}

CASTED.summary.contributors <- function(CASTED.committers.count){
    # Get summary data of contribution ratios of each repository
    #
    # Args:
    #   CASTED.committers.count: Output table from that function
    #
    # Returns:
    #   CASTED: DF of (owner's contribution rate, # majors, # minors)

    # Just for making the coding simpler
    DF <- CASTED.committers.count

    # (1) Get the maximum contribution rate (Performance of Owner)
    CASTED1 <- suppressWarnings(dcast(data = DF, project ~ ., fun.aggregate=max, value.var = "ratio"))
    names(CASTED1)[2] <- "ownership"

    # (2) Get the number of major contributors
    CASTED2 <- dcast(data = DF, project ~ ., fun.aggregate=sum, value.var = "major")
    names(CASTED2)[2] <- "major"

    # (3) Get the number of minor contributors
    CASTED3 <- dcast(data = DF, project ~ ., fun.aggregate=sum, value.var = "minor")
    names(CASTED3)[2] <- "minor"

    # (4) Get the LT (Laakso-Taagepera)
    DF.subset <- subset(DF, select=c(project, ratio))
    DF.subset$lt <- DF.subset$ratio^2
    CASTED4 <- suppressWarnings(dcast(data = DF.subset, project ~ ., fun.aggregate=sum, value.var = "lt"))
    names(CASTED4)[2] <- "lt"
    CASTED4$lt <- round(1 / CASTED4$lt, 3)

    # (5) Put everything in one table
    T <- merge(CASTED1, CASTED2, by="project")
    T <- merge(T, CASTED3, by="project")
    T <- merge(T, CASTED4, by="project")
    T
}

CASTED.review.count <- function(REVIEW){
    # Count number of comments in REVIEW and categorize them in 'class'
    #
    # Args:
    #   REVIEW: REVIEW table with 'project' column
    #
    # Returns:
    #   CASTED: DF of counted number of each class for projects
    TMP <- as.data.frame(table(REVIEW$class, REVIEW$project))
    names(TMP) <- c("class", "project", "Freq")
    dcast(TMP, project ~ class, fun.aggregate=sum, value.var="Freq")
}

CASTED.reviewer.orig <- function(reviewed.CHANGE, TABLE.committers.count){
    # Summarize reviewer's expertise info of repositories
    #
    # Args:
    #   reviewed.CHANGE <- CHANGE.summary.REVIEW(CHANGE, PATCHSET, REVIEW)
    #   TABLE.committers.count <- CASTED.committers.count <- function(PATCHSET)
    #
    # Returns:
    #   CASTED: (project, review.by.exp, typical.exp)
    #           whereas review.by.exp is the ratio of such kind of all commits
    #           and typial.exp is the median of approving person's contributions

    # X: Intermediate casted table, Approver ~ Project ~ Freq (of approval)
    # Approver is whoever once approved a change in the entire projects group
    # Most of rows have Freq 0
    #         1 Akihiro Shimomura device/qcom/common    0
    #         2      Akihiro Taka device/qcom/common    0
    X <- as.data.frame(table(reviewed.CHANGE$approver, reviewed.CHANGE$project))
    casted <- dcast(data = X, formula = Var1 ~ Var2, value.var = "Freq")
    row.names(casted) <- casted$Var1

    # Y: PROJECT:APPROVER table with Review frequenncy
    # Filtered X with non-zero Freq
    # e.g.
    #       Fen Wang device/qcom/common    3        device/qcom/common:Fen Wang
    #  Satoshi Obata device/qcom/common    6   device/qcom/common:Satoshi Obata
    Y <- subset(X, Freq!=0)
    names(Y) <- c("approver", "project", "Freq")
    Y$id <- paste(Y$project, Y$approver, sep=":")

    # Z: PROJECT:COMMITTER table with Commit frequency
    # Not an approver based, commit based to get 'major/minor/# commits'
    # project ~ Name ~ metrics
    Z <- TABLE.committers.count
    Z$id <- paste(Z$project, Z$committer, sep=":")
    names(Z)[which(names(Z)=="Freq")] <- "my.commits"

    # A: Extention of Y with Z info
    # Merging Approver table with committer table, Approving freq and Commit freq, Major is shown
    # A record must be someone who can approve a change
    A <- merge(Y, Z[, c("id", "committer", "my.commits", "major")], by="id", all.x=TRUE, all.y=FALSE)
    A[is.na(A$my.commits),]$my.commits <- 0
    A.subset <- A[!is.na(A$major),]
    A.subset <- A.subset[A.subset$major==TRUE,]

    # B: CHANGE table with limited info
    # Change table to get 'review.by.exp' TRUE/FALSE by looking at "A"
    B <- subset(reviewed.CHANGE, select=c("project", "approver", "display.name"))
    B$id.approver <- paste(B$project, B$approver, sep=":")
    B$id.committer <- paste(B$project, B$display.name, sep=":")
    B <- subset(B, select=-c(approver,display.name))
    B$review.by.exp <- mapply(m=B$id.approver, n=B$id.committer,
                              FUN=function(m, n){
                                  ans1 <- m %in% A.subset$id
                                  ans2 <- n %in% A.subset$id
                                  as.logical(ans1 + ans2)
                              }
    )

    # C: Map the expertise (# commits by the approver, typical reviewer's exp.) to CHANGE table
    # A record of commit is approved by whom? how many commits have the approvers made commits ever?
    C <- merge(subset(B, select=c(project, id.approver)), subset(A, select=c(id, my.commits)),
               by.x="id.approver", by.y="id", all.x=TRUE, all.y=FALSE)

    # D: Map the expert involvement in REVIEW table
    # A record is a unique CHANGE whether this is reviewed by EXP, if a committer is major?
    # "exp" is either of them
    D <- merge(B[, c("id.committer", "review.by.exp")],
               Z[, c("id", "project", "major")],
               by.x="id.committer", by.y="id", all.x=TRUE, all.y=FALSE)
    D$exp <- as.logical(D$review.by.exp + D$major)

    CASTED <- aggregate(x=D[,c("exp" ,"review.by.exp")], by=list(D$project), FUN=sum)
    names(CASTED) <- c("project", "exp.involve", "exp.review")
    TMP <- aggregate(x=C[, "my.commits"], by=list(C$project), FUN=median)
    names(TMP) <- c("project", "typical.exp")
    CASTED <- merge(CASTED, TMP, by="project", all=TRUE)
    CASTED
}


CASTED.review.self <- function(CHANGE.summary.REVIEW){
    # Measure self.approval/verification ratio of each component
    #
    # Args:
    #   CHANGE: Output from CHANGE.summary.REVIEW
    #
    # Returns:
    #   CASTED: (project, self.approval.rate, self.verification.rate)
    aggregate(CHANGE.summary.REVIEW[, c("self.approval", "self.verification")],
              by=list(project=CHANGE.summary.REVIEW$project), sum)
}

CASTED.normalized.review <- function(CHANGE.summarized.REVIEW, CHANGE.reviewed.effectiveness){
    # Measure statistical properties of review window
    #
    # Args:
    #   CHANGE: CHANGE table
    #
    # Returns:
    #   CASTED: (project, length.discuss, times.discuss, review.window,
    #            no.discuss, reviewers, patches.sd, hastily)
    #
    # Notes:
    #   - `length.discuss` is calculated by
    #     (length of comments) + (number of inlines) * 3
    #     as there is a limitation of getting the inline text information and the
    #     inline comments tend to be around 3 words
    #   - `times.discuss` is the number of times reviewers made comments
    #     excluding the committer her/himself.
    agg0 <- CHANGE.reviewed.effectiveness # agg0 <- TMP9
    C1 <- CHANGE.summarized.REVIEW #  C1 <- RC0
    C1 <- merge(C1, agg0, by="number")
    if(sum(C1$churn==0)!=0){
        C1[C1$churn==0, ]$churn <- 1
    }
    C1$length.discuss <- (C1$L.inlines.others * 3 + C1$L.comments.others) / C1$churn
    C1$times.discuss <- C1$N.comments.others
    C1$review.window <- as.numeric(C1$date_closed - C1$date_created, units="hours") / C1$churn
    C1$hastily <- (C1$review.window < (1/200)) # If a one hour review contains more than 200 lines.
                                               # 'review.window' is normalized by code churn so
                                               # it is a spent-review time for one line.
                                               # The limit is 1/200 ~ 0.3 minute
    C1$no.discuss <- 0 == (C1$N.comments.others + C1$N.inlines.others)
    C1$reviewers <- C1$N.reviewersq

    TMP1 <- C1[, c("project", "length.discuss", "times.discuss", "review.window")]
    agg1 <- aggregate(subset(TMP1, select=-c(project)), by=list(project=TMP1$project), FUN=median)
    TMP2 <- C1[, c("project", "no.discuss", "N.reviewers", "patches.sd", "hastily")]
    agg2 <- aggregate(subset(TMP2, select=-c(project)), by=list(project=TMP2$project), FUN=sum)
    names(agg2)[3] <- "reviewers"
    merge(agg1, agg2, by="project")
}

GERRIT.add.ID <- function(GERRIT){
    GERRIT$id <- paste(GERRIT$number, GERRIT$revision_number, sep="/")
    GERRIT
}

GERRIT.use.revision_number <- function(GERRIT){
    names(GERRIT)[which(names(GERRIT)=="num_patches")] <- "revision_number"
    GERRIT
}

INDEX.proj <- function(GERRIT, MAP.num.proj){
    INDEX <- MAP.num.proj
    merge(GERRIT, INDEX, all.x=TRUE, all.y=FALSE, by="number")
}

FLTR.latest <- function(GERRIT){
    G <- GERRIT
    G$id <- paste(G$number, G$revision_number, sep="/")
    agg <- aggregate(x=G$revision_number, by=list(number=G$number), FUN=max)
    names(agg) <- c("number", "revision_number")
    agg$id <- paste(agg$number, agg$revision_number, sep="/")
    TMP <- merge(G, subset(agg, select=c(id)), all.x=FALSE, all.y=TRUE, by="id")
    subset(TMP, select=-c(id))
}

FLTR.number <- function(GERRIT, numbers){
    subset(GERRIT, number %in% numbers)
}