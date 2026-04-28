import {
  Button,
  Collapsible,
  Flex,
  ProgressBar,
  Section,
  Tooltip,
} from 'tgui-core/components';
import { BooleanLike } from 'tgui-core/react';

import { useBackend } from '../backend';
import { Window } from '../layouts';

type MutationBarData = {
  mutation_points_available: number;
  mutation_points_used: number;
  mutation_points_maximum: number;
};

type MutationData = {
  mutation_categories: string[];
  mutations: MutationIndividualData[];
};

type MutationIndividualData = {
  category: string;
  name: string;
  description: string;
  typepath: string;
  owned: BooleanLike;
};

export const MutationSelector = (_props: any) => {
  const { data } = useBackend<MutationData>();
  const { mutations, mutation_categories } = data;

  return (
    <Window theme="xeno" width={500} height={600}>
      <Window.Content scrollable>
        <Section title="Mutation Evolution" key="Mutation Evolution">
          <MutationBar />
        </Section>
        {mutation_categories &&
          mutation_categories.map((category_name) => (
            <MutationSection
              key={category_name}
              category_name={category_name}
              mutations={mutations}
            />
          ))}
      </Window.Content>
    </Window>
  );
};

const MutationBar = (_props: any) => {
  const { data } = useBackend<MutationBarData>();
  const {
    mutation_points_available,
    mutation_points_used,
    mutation_points_maximum,
  } = data;

  let tooltipContent = 'You are ready to buy another mutation.';
  if (mutation_points_used >= mutation_points_maximum) {
    tooltipContent = 'You have the maximum amount of mutations!';
  } else if (mutation_points_used >= mutation_points_available) {
    tooltipContent = "You can't buy another mutation yet...";
  }

  return (
    <Tooltip content={tooltipContent}>
      <Flex mb={1}>
        <Flex.Item grow>
          <ProgressBar
            color="green"
            value={mutation_points_available / mutation_points_maximum}
          >
            {`${mutation_points_available} / ${mutation_points_maximum} `}
          </ProgressBar>
        </Flex.Item>
      </Flex>
    </Tooltip>
  );
};

const MutationSection = (props: {
  category_name: string;
  mutations: MutationIndividualData[];
}) => {
  const { act, data } = useBackend<MutationBarData>();
  const { mutation_points_available, mutation_points_used } = data;

  return (
    <Collapsible title={`${props.category_name} Mutations`}>
      {props.mutations &&
        props.mutations
          .filter((mutation) => mutation.category === props.category_name)
          .map((mutation) => (
            <Section
              title={`${mutation.name}`}
              mb={1}
              key={mutation.name}
              buttons={
                <Button
                  content={`Buy`}
                  key={mutation.name}
                  onClick={() =>
                    act('purchase', { mutation_typepath: mutation.typepath })
                  }
                  disabled={
                    mutation_points_used >= mutation_points_available ||
                    mutation.owned
                  }
                  selected={mutation.owned}
                />
              }
            >
              <Flex direction="column-reverse" align={'left'}>
                {mutation.description}
              </Flex>
            </Section>
          ))}
    </Collapsible>
  );
};
