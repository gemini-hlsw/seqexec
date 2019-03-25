/*
 * Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.epics.acm;

import edu.gemini.epics.acm.generated.*;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Marshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.*;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class converts the old channel access configuration files used by <b>ocswish</b> into the new XML based format.
 * It can be used as a stand-alone program.
 * <p/>
 * It imposes some restriction on the old format. It can process only one file at a time: include directives are
 * ignored. Command parameters must have the same common prefix as the associated apply record.
 * <p/>
 * The conversion tries to figure out when a parameter exist only to mark the CAD, and excludes them, using the channel
 * name to configure the command sender instead.
 * <p/>
 * All command parameters and status attributes are assigned the type STRING.
 *
 * @author jluhrs
 */
public final class CaConfigFileConverter {

    private static final Logger LOG = LoggerFactory.getLogger(CaConfigFileConverter.class.getName());

    private static final String USAGE = "Usage: java edu.gemini.epics.acm.CaConfigFileConverter [-f outFile] [inFiles]";

    private static final String CONFIG_SCHEMA_FILE = "/CaSchema.xsd";
    private static final String XMLSCHEMA_URL = "http://www.w3.org/2001/XMLSchema";
    private static final String DIR_PARAM = "DIR";
    private static final String DUMMY_PARAM = "dummy";

    public static void convert(BufferedReader in, Writer out)
            throws IOException {
        List<BufferedReader> inputList = new ArrayList<>();
        inputList.add(in);
        convert(inputList, out);
    }

    private static void convert(List<BufferedReader> inputList, Writer out)
            throws IOException {

        Records records = new Records();
        Set<ApplyDef> applies = new HashSet<>();
        Set<CommandDef> commands = new HashSet<>();
        Set<StatusDef> statuses = new HashSet<>();

        String line;

        for (BufferedReader in : inputList) {
            while ((line = readLine(in)) != null) {
                if (readApply(line, applies)) {
                    continue;
                }
                if (readCommand(line, in, commands)) {
                    continue;
                }
                readStatus(line, in, statuses);
            }
        }

        // This set collects the tops discovered while building the records.
        Set<String> tops = new HashSet<>();

        // Build the status acceptors.
        buildStatusAcceptors(records, statuses, tops);

        // Build the apply and command senders
        buildAppliesAndCommands(records, applies, commands, tops);

        // Finally, fill in the Top definitions.
        for (String topDef : tops) {
            TopType top = new TopType();
            top.setName(topDef);
            top.setValue(topDef);
            records.getTop().add(top);
        }

        try {
            JAXBContext jc = JAXBContext.newInstance(Records.class);
            Marshaller marshaller = jc.createMarshaller();
            SchemaFactory factory = SchemaFactory.newInstance(XMLSCHEMA_URL);
            Schema schema;
            schema = factory.newSchema(CaConfigFileConverter.class.getResource(CONFIG_SCHEMA_FILE));
            marshaller.setSchema(schema);
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
            marshaller.marshal(records, out);
        } catch (Exception e) {
            LOG.warn(e.getMessage());
        }

    }

    private static void buildAppliesAndCommands(Records records, Set<ApplyDef> applies, Set<CommandDef> commands,
                                                Set<String> tops) {
        // First, collect the commands for each apply
        Map<String, HashSet<CommandDef>> commandMap = new HashMap<>();
        for (CommandDef commandDef : commands) {
            if (!commandMap.containsKey(commandDef.apply)) {
                commandMap.put(commandDef.apply, new HashSet<CommandDef>());
            }
            commandMap.get(commandDef.apply).add(commandDef);
        }

        for (ApplyDef applyDef : applies) {
            // Ignore applies that don't have commands
            if (commandMap.containsKey(applyDef.name)) {
                String applyTop = null;
                String carTop;
                String applyRecord;
                String carRecord;
                if (applyDef.applyRecord.split(":").length > 0) {
                    applyTop = applyDef.applyRecord.split(":")[0];
                    applyRecord = applyDef.applyRecord.substring(applyTop
                            .length() + 1);
                    carTop = applyDef.carRecord.split(":")[0];
                    carRecord = applyDef.carRecord
                            .substring(applyTop.length() + 1);

                    if (!applyTop.equals(carTop)) {
                        System.err
                                .println("Rejected apply sender "
                                        + applyDef.name
                                        + "apply and CAR record have different prefixes.");
                        continue;
                    }
                } else {
                    applyRecord = applyDef.applyRecord;
                    carRecord = applyDef.carRecord;
                }
                ApplyType apply = new ApplyType();
                apply.setName(applyDef.name);
                if (applyTop != null) {
                    apply.setTop(applyTop);
                    tops.add(applyTop);
                }
                apply.setApply(applyRecord);
                apply.setCar(carRecord);
                apply.setDescription(applyDef.description);

                for (CommandDef commandDef : commandMap.get(applyDef.name)) {
                    // Commands should have at least one parameter. If the
                    // parameter is named dummy, or if its channel ends in .DIR,
                    // then it is used as the base record name.
                    if (!commandDef.params.isEmpty()) {
                        CommandType command = new CommandType();
                        command.setName(commandDef.name);
                        command.setDescription(commandDef.description);

                        for (ParamDef paramDef : commandDef.params) {
                            String paramTop = null;
                            String paramRecord;
                            String recordParam = null;
                            if (paramDef.channel.split(":").length > 0) {
                                paramTop = paramDef.channel.split(":")[0];
                                paramRecord = paramDef.channel
                                        .substring(paramTop.length() + 1);
                                String[] splitRecord = paramRecord.split("\\.");
                                if (splitRecord.length > 0) {
                                    recordParam = splitRecord[splitRecord.length - 1];
                                }
                            } else {
                                paramRecord = paramDef.channel;
                            }

                            // Reject parameters with a different top
                            if (applyTop != null && !applyTop.equals(paramTop)) {
                                System.err
                                        .println("Rejected parameter "
                                                + paramDef.name
                                                + " for command sender "
                                                + commandDef.name
                                                + ": parameter top is different from apply top.");
                                continue;
                            }
                            if (DIR_PARAM.equals(recordParam)
                                    || DUMMY_PARAM.equals(paramDef.name)) {
                                if (command.getRecord() != null) {
                                    System.err
                                            .println("Rejected parameter "
                                                    + paramDef.name
                                                    + " for command sender "
                                                    + commandDef.name
                                                    + ": command record already defined.");
                                } else {
                                    // Note that DIR_PARAM.equals(recordParam)
                                    // => commandRecord!=null
                                    command.setRecord(paramRecord.substring(
                                            0,
                                            paramRecord.length()
                                                    - (recordParam != null ? (recordParam
                                                    .length() + 1) : 0)));
                                }
                                continue;
                            }
                            CommandType.Parameter param = new CommandType.Parameter();
                            param.setName(paramDef.name);
                            param.setChannel(paramRecord);
                            param.setDescription(paramDef.description);
                            param.setType(DataType.STRING);
                            command.getParameter().add(param);
                        }
                        if (command.getRecord() != null
                                || !command.getParameter().isEmpty()) {
                            apply.getCommand().add(command);
                        } else {
                            System.err.println("Reject command sender "
                                    + commandDef.name
                                    + ": no valid parameters.");
                        }
                    }
                }
                if (!apply.getCommand().isEmpty()) {
                    records.getApply().add(apply);
                } else {
                    System.err.println("Reject apply sender " + applyDef.name
                            + ": no valid commands.");
                }
            }
        }
    }

    private static void buildStatusAcceptors(Records records, Set<StatusDef> statuses, Set<String> tops) {
        for (StatusDef statusDef : statuses) {
            String top = null;
            StatusType status = new StatusType();
            status.setName(statusDef.name);
            status.setDescription(statusDef.description);
            for (AttribDef attrDef : statusDef.attribs) {
                String attrTop = null;
                String channel;
                if (attrDef.channel.split(":").length > 0) {
                    attrTop = attrDef.channel.split(":")[0];
                    channel = attrDef.channel.substring(attrTop.length() + 1);
                } else {
                    channel = attrDef.channel;
                }
                StatusType.Attribute attr = new StatusType.Attribute();
                attr.setName(attrDef.name);
                attr.setChannel(channel);
                attr.setType(DataType.STRING);
                attr.setDescription(attrDef.description);
                if (top == null) {
                    if (attrTop != null) {
                        top = attrTop;
                        tops.add(top);
                        status.setTop(top);
                    }
                } else {
                    if (!top.equals(attrTop)) {
                        if (attrTop != null) {
                            tops.add(attrTop);
                            attr.setTop(attrTop);
                        } else {
                            attr.setTop("");
                        }
                    }
                }
                status.getAttribute().add(attr);
            }
            records.getStatus().add(status);
        }
    }

    private static boolean readStatus(String line, BufferedReader in,
                                      Set<StatusDef> statuses) throws IOException {
        final String statusPatternStr = "^\\s*status\\s*(?<name>[\\w_]+[\\w.:_]*)\\s*(?<description>\\w.*)?\\s*$";
        final String startBlockPatternStr = "^\\s*\\{\\s*$";
        final String endBlockPatternStr = "^\\s*\\}\\s*$";
        final String attrPatternStr = "^\\s*(?<name>[\\w_]+[\\w.:_]*)\\s*(?<channel>[\\w.:_]+)\\s*(?:\\d+)\\s*(?<description>.*)\\s*$";
        Pattern statusPattern = Pattern.compile(statusPatternStr);
        Pattern startBlockPattern = Pattern.compile(startBlockPatternStr);
        Pattern endBlockPattern = Pattern.compile(endBlockPatternStr);
        Pattern attrPattern = Pattern.compile(attrPatternStr);

        Matcher statusMatcher = statusPattern.matcher(line);
        if (statusMatcher.matches()) {
            // First, search for block start
            if ((line = readLine(in)) != null
                    && startBlockPattern.matcher(line).matches()) {
                Set<AttribDef> attribs = new HashSet<>();
                while ((line = readLine(in)) != null) {
                    Matcher attrMatcher = attrPattern.matcher(line);
                    if (attrMatcher.matches()) {
                        attribs.add(new AttribDef(attrMatcher.group("name"),
                                attrMatcher.group("channel"), attrMatcher
                                .group("description")));
                    } else {
                        statuses.add(new StatusDef(statusMatcher.group("name"),
                                statusMatcher.group("description"), attribs));
                        if (endBlockPattern.matcher(line).matches()) {
                            return true;
                        }
                    }
                }
            }

            throw new IllegalArgumentException("Error parsing file.");
        } else {
            return false;
        }
    }

    private static boolean readCommand(String line, BufferedReader in,
                                       Set<CommandDef> commands) throws IOException {
        final String commandPatternStr = "^\\s*command\\s*(?<name>[\\w_]+[\\w.:_]*)\\s*(?<apply>[\\w_]+[\\w.:_]*)\\s*(?<description>\\w.*)?\\s*$";
        final String startBlockPatternStr = "^\\s*\\{\\s*$";
        final String endBlockPatternStr = "^\\s*\\}\\s*$";
        final String paramPatternStr = "^\\s*(?<name>[\\w_]+[\\w.:_]*)\\s*(?<channel>[\\w.:_]+)\\s*(?<description>.*)\\s*$";
        Pattern commandPattern = Pattern.compile(commandPatternStr);
        Pattern startBlockPattern = Pattern.compile(startBlockPatternStr);
        Pattern endBlockPattern = Pattern.compile(endBlockPatternStr);
        Pattern paramPattern = Pattern.compile(paramPatternStr);

        Matcher commandMatcher = commandPattern.matcher(line);
        if (commandMatcher.matches()) {
            // First, search for block start
            if ((line = readLine(in)) != null
                    && startBlockPattern.matcher(line).matches()) {
                Set<ParamDef> params = new HashSet<>();
                while ((line = readLine(in)) != null) {
                    Matcher paramMatcher = paramPattern.matcher(line);
                    if (paramMatcher.matches()) {
                        params.add(new ParamDef(paramMatcher.group("name"),
                                paramMatcher.group("channel"), paramMatcher
                                .group("description")));
                    } else {
                        commands.add(new CommandDef(commandMatcher
                                .group("name"), commandMatcher.group("apply"),
                                commandMatcher.group("description"), params));
                        if (endBlockPattern.matcher(line).matches()) {
                            return true;
                        }
                    }
                }
            }

            throw new IllegalArgumentException("Error parsing file.");
        } else {
            return false;
        }
    }

    private static boolean readApply(String line,
                                     Set<ApplyDef> applies) {

        final String applyPatternStr = "^\\s*apply\\s*(?<name>[\\w_]+[\\w.:_]*)\\s*(?<apply>[\\w.:_]+)\\s*(?<car>[\\w.:_]+)\\s*(?<description>\\w.*)?\\s*$";
        Pattern applyPattern = Pattern.compile(applyPatternStr);

        Matcher applyMatcher = applyPattern.matcher(line);
        if (applyMatcher.matches()) {
            applies.add(new ApplyDef(applyMatcher.group("name"), applyMatcher
                    .group("apply"), applyMatcher.group("car"), applyMatcher
                    .group("description")));
            return true;
        } else {
            return false;
        }
    }

    private static String readLine(BufferedReader in) throws IOException {
        final String commentLinePatternStr = "^\\s*#.*$";
        final String blankLinePatternStr = "^\\s*$";
        Pattern commentLinePattern = Pattern.compile(commentLinePatternStr);
        Pattern blankLinePattern = Pattern.compile(blankLinePatternStr);
        String line;
        while ((line = in.readLine()) != null) {
            if (!commentLinePattern.matcher(line).matches()
                    && !blankLinePattern.matcher(line).matches()) {
                return line;
            }
        }
        return null;
    }

    public static void main(String[] args) {
        List<BufferedReader> input = new ArrayList<>();
        Writer output = null;

        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "--help":
                case "-h":
                    System.out.println(USAGE);
                    System.exit(0);
                case "-f":
                    i++;
                    if (i >= args.length) {
                        System.out.println(USAGE);
                        System.exit(-1);
                    }
                    try {
                        output = new BufferedWriter(new FileWriter(args[i]));
                    } catch (IOException e) {
                        LOG.warn(e.getMessage());
                        System.exit(-1);
                    }
                    break;
                default:
                    try {
                        input.add(new BufferedReader(new FileReader(args[i])));
                    } catch (FileNotFoundException e) {
                        LOG.warn(e.getMessage());
                        System.exit(-1);
                    }
                    break;
            }
        }

        if (input.isEmpty()) {
            input.add(new BufferedReader(new InputStreamReader(System.in)));
        }
        if (output == null) {
            output = new BufferedWriter(new OutputStreamWriter(System.out));
        }

        try {
            convert(input, output);
        } catch (IOException e) {
            LOG.warn(e.getMessage());
            System.exit(-1);
        }

    }

    private static final class ApplyDef {
        public final String name;
        public final String applyRecord;
        public final String carRecord;
        public final String description;

        public ApplyDef(String name, String applyRecord, String carRecord,
                        String description) {
            this.name = name;
            this.applyRecord = applyRecord;
            this.carRecord = carRecord;
            this.description = description;
        }
    }

    private static final class ParamDef {
        public final String name;
        public final String channel;
        public final String description;

        public ParamDef(String name, String channel, String description) {
            super();
            this.name = name;
            this.channel = channel;
            this.description = description;
        }
    }

    private static final class CommandDef {
        public final String name;
        public final String apply;
        public final String description;
        public final Set<ParamDef> params;

        public CommandDef(String name, String apply, String description,
                          Set<ParamDef> params) {
            super();
            this.name = name;
            this.apply = apply;
            this.description = description;
            this.params = params;
        }
    }

    private static final class AttribDef {
        public final String name;
        public final String channel;
        public final String description;

        public AttribDef(String name, String channel, String description) {
            super();
            this.name = name;
            this.channel = channel;
            this.description = description;
        }
    }

    private static final class StatusDef {
        public final String name;
        public final String description;
        public final Set<AttribDef> attribs;

        public StatusDef(String name, String description, Set<AttribDef> attribs) {
            super();
            this.name = name;
            this.description = description;
            this.attribs = attribs;
        }

    }

}
